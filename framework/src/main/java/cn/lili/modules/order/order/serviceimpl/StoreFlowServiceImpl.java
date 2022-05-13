package cn.lili.modules.order.order.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.dto.StoreFlowQueryDTO;
import cn.lili.modules.order.order.entity.enums.FlowTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.mapper.StoreFlowMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.service.StoreFlowService;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.service.RefundLogService;
import cn.lili.modules.store.entity.dos.Bill;
import cn.lili.modules.store.entity.vos.StoreFlowPayDownloadVO;
import cn.lili.modules.store.entity.vos.StoreFlowRefundDownloadVO;
import cn.lili.modules.store.service.BillService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 商家订单流水业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 7:38 下午
 */
@Slf4j
@Service
public class StoreFlowServiceImpl extends ServiceImpl<StoreFlowMapper, StoreFlow> implements StoreFlowService {

    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;
    /**
     * 订单货物
     */
    @Autowired
    private OrderItemService orderItemService;
    /**
     * 退款日志
     */
    @Autowired
    private RefundLogService refundLogService;

    @Autowired
    private BillService billService;

    /**
     * 店铺订单支付流水
     * @param orderSn 订单编号
     */
    @Override
    public void payOrder(String orderSn) {
        //根据订单编号获取子订单列表
        List<OrderItem> orderItems = orderItemService.getByOrderSn(orderSn);
        //根据订单编号获取订单数据
        Order order = orderService.getBySn(orderSn);
        //获取订单促销类型,如果为促销订单则获取促销商品并获取结算价
        String orderPromotionType = order.getOrderPromotionType();

        //循环子订单记录流水
        for (OrderItem item : orderItems) {
            StoreFlow storeFlow = new StoreFlow();
            BeanUtil.copyProperties(item, storeFlow);

            //去掉orderitem的时间。
            storeFlow.setCreateTime(null);
            //入账
            storeFlow.setId(SnowFlake.getIdStr());
            storeFlow.setFlowType(FlowTypeEnum.PAY.name());
            storeFlow.setSn(SnowFlake.createStr("SF"));
            storeFlow.setOrderSn(item.getOrderSn());
            storeFlow.setOrderItemSn(item.getSn());
            storeFlow.setStoreId(order.getStoreId());
            storeFlow.setStoreName(order.getStoreName());
            storeFlow.setMemberId(order.getMemberId());
            storeFlow.setMemberName(order.getMemberName());
            storeFlow.setGoodsName(item.getGoodsName());
            storeFlow.setOrderPromotionType(item.getPromotionType());
            //格式化订单价格详情
            PriceDetailDTO priceDetailDTO = JSONUtil.toBean(item.getPriceDetail(), PriceDetailDTO.class);
            //站点优惠券比例=最大比例(100)-店铺承担比例
            storeFlow.setSiteCouponPoint(CurrencyUtil.sub(100,priceDetailDTO.getSiteCouponPoint()));
            //平台优惠券 使用金额
            storeFlow.setSiteCouponPrice(priceDetailDTO.getSiteCouponPrice());
            //站点优惠券佣金（站点优惠券承担金额=优惠券金额 * (站点承担比例/100)）
            storeFlow.setSiteCouponCommission(CurrencyUtil.mul(storeFlow.getSiteCouponPrice(),CurrencyUtil.div(storeFlow.getSiteCouponPoint(),100)));

            /**
             * @TODO 计算平台佣金
             */
            //店铺流水金额=goodsPrice(商品总金额（商品原价）)+ freightPrice(配送费) - discountPrice(优惠金额) - couponPrice(优惠券金额) + updatePrice(订单修改金额)
            storeFlow.setFinalPrice(item.getPriceDetailDTO().getFlowPrice());
            //平台收取交易佣金=(flowPrice(流水金额) * platFormCommissionPoint(平台佣金比例))/100
            storeFlow.setCommissionPrice(item.getPriceDetailDTO().getPlatFormCommission());
            //单品分销返现支出
            storeFlow.setDistributionRebate(item.getPriceDetailDTO().getDistributionCommission());
            //最终结算金额=flowPrice(流水金额) - platFormCommission(平台收取交易佣金) - distributionCommission(单品分销返现支出)
            storeFlow.setBillPrice(item.getPriceDetailDTO().getBillPrice());
            //兼容为空，以及普通订单操作
            if (CharSequenceUtil.isNotEmpty(orderPromotionType)) {
                if (orderPromotionType.equals(OrderPromotionTypeEnum.NORMAL.name())) {
                    //普通订单操作
                }
                //如果为砍价活动，填写砍价结算价
                else if (orderPromotionType.equals(OrderPromotionTypeEnum.KANJIA.name())) {
                    storeFlow.setKanjiaSettlementPrice(item.getPriceDetailDTO().getSettlementPrice());
                }
                //如果为砍价活动，填写砍价结算价
                else if (orderPromotionType.equals(OrderPromotionTypeEnum.POINTS.name())) {
                    storeFlow.setPointSettlementPrice(item.getPriceDetailDTO().getSettlementPrice());
                }
            }
            //添加支付方式
            storeFlow.setPaymentName(order.getPaymentMethod());
            //添加第三方支付流水号
            storeFlow.setTransactionId(order.getReceivableNo());

            //添加付款交易流水
            this.save(storeFlow);
        }
    }


    /**
     * 店铺订单退款流水
     * @param afterSale 售后单
     */
    @Override
    public void refundOrder(AfterSale afterSale) {
        StoreFlow storeFlow = new StoreFlow();
        //退款
        storeFlow.setFlowType(FlowTypeEnum.REFUND.name());
        storeFlow.setSn(SnowFlake.createStr("SF"));
        storeFlow.setRefundSn(afterSale.getSn());
        storeFlow.setOrderSn(afterSale.getOrderSn());
        storeFlow.setOrderItemSn(afterSale.getOrderItemSn());
        storeFlow.setStoreId(afterSale.getStoreId());
        storeFlow.setStoreName(afterSale.getStoreName());
        storeFlow.setMemberId(afterSale.getMemberId());
        storeFlow.setMemberName(afterSale.getMemberName());
        storeFlow.setGoodsId(afterSale.getGoodsId());
        storeFlow.setGoodsName(afterSale.getGoodsName());
        storeFlow.setSkuId(afterSale.getSkuId());
        storeFlow.setImage(afterSale.getGoodsImage());
        storeFlow.setSpecs(afterSale.getSpecs());


        //获取付款信息
        StoreFlow payStoreFlow = this.getOne(new LambdaUpdateWrapper<StoreFlow>().eq(StoreFlow::getOrderItemSn, afterSale.getOrderItemSn())
                .eq(StoreFlow::getFlowType, FlowTypeEnum.PAY));
        //申请商品退款数量
        storeFlow.setNum(afterSale.getNum());
        //分类ID
        storeFlow.setCategoryId(payStoreFlow.getCategoryId());
        //佣金 = （佣金/订单商品数量）* 售后商品数量
        storeFlow.setCommissionPrice(CurrencyUtil.mul(CurrencyUtil.div(payStoreFlow.getCommissionPrice(), payStoreFlow.getNum()), afterSale.getNum()));
        //分销佣金 =（分销佣金/订单商品数量）* 售后商品数量
        storeFlow.setDistributionRebate(CurrencyUtil.mul(CurrencyUtil.div(payStoreFlow.getDistributionRebate(), payStoreFlow.getNum()), afterSale.getNum()));
        //流水金额 = 实际退款金额
        storeFlow.setFinalPrice(afterSale.getActualRefundPrice());
        //最终结算金额 =店铺流水金额+店铺单品返现支出金额+平台收取佣金金额
        storeFlow.setBillPrice(CurrencyUtil.add(storeFlow.getFinalPrice(), storeFlow.getDistributionRebate(), storeFlow.getCommissionPrice()));
        //站点优惠券补贴返还金额=(站点优惠券补贴金额/购买商品数量)*退款商品数量
        storeFlow.setSiteCouponCommission(CurrencyUtil.mul(CurrencyUtil.div(payStoreFlow.getSiteCouponCommission(),payStoreFlow.getNum()),afterSale.getNum()));
        //平台优惠券 使用金额
        storeFlow.setSiteCouponPrice(payStoreFlow.getSiteCouponPrice());
        //站点优惠券佣金比例
        storeFlow.setSiteCouponPoint(payStoreFlow.getSiteCouponPoint());
        //退款日志
        RefundLog refundLog = refundLogService.queryByAfterSaleSn(afterSale.getSn());
        //第三方流水单号
        storeFlow.setTransactionId(refundLog.getReceivableNo());
        //支付方式
        storeFlow.setPaymentName(refundLog.getPaymentName());
        this.save(storeFlow);
    }

    @Override
    public IPage<StoreFlow> getStoreFlow(StoreFlowQueryDTO storeFlowQueryDTO) {

        return this.page(PageUtil.initPage(storeFlowQueryDTO.getPageVO()), generatorQueryWrapper(storeFlowQueryDTO));
    }

    @Override
    public StoreFlow queryOne(StoreFlowQueryDTO storeFlowQueryDTO) {
        return this.getOne(generatorQueryWrapper(storeFlowQueryDTO));
    }

    @Override
    public List<StoreFlowPayDownloadVO> getStoreFlowPayDownloadVO(StoreFlowQueryDTO storeFlowQueryDTO) {
        return baseMapper.getStoreFlowPayDownloadVO(generatorQueryWrapper(storeFlowQueryDTO));
    }

    @Override
    public List<StoreFlowRefundDownloadVO> getStoreFlowRefundDownloadVO(StoreFlowQueryDTO storeFlowQueryDTO) {
        return baseMapper.getStoreFlowRefundDownloadVO(generatorQueryWrapper(storeFlowQueryDTO));
    }


    @Override
    public IPage<StoreFlow> getStoreFlow(String id, String type, PageVO pageVO) {
        Bill bill = billService.getById(id);
        return this.getStoreFlow(StoreFlowQueryDTO.builder().type(type).pageVO(pageVO).bill(bill).build());
    }

    @Override
    public IPage<StoreFlow> getDistributionFlow(String id, PageVO pageVO) {
        Bill bill = billService.getById(id);
        return this.getStoreFlow(StoreFlowQueryDTO.builder().pageVO(pageVO).bill(bill).build());
    }

    @Override
    public List<StoreFlow> listStoreFlow(StoreFlowQueryDTO storeFlowQueryDTO) {
        return this.list(generatorQueryWrapper(storeFlowQueryDTO));
    }

    /**
     * 生成查询wrapper
     *
     * @param storeFlowQueryDTO 搜索参数
     * @return 查询wrapper
     */
    private LambdaQueryWrapper generatorQueryWrapper(StoreFlowQueryDTO storeFlowQueryDTO) {


        LambdaQueryWrapper<StoreFlow> lambdaQueryWrapper = Wrappers.lambdaQuery();
        //分销订单过滤是否判定
        lambdaQueryWrapper.isNotNull(storeFlowQueryDTO.getJustDistribution() != null && storeFlowQueryDTO.getJustDistribution(),
                StoreFlow::getDistributionRebate);

        //流水类型判定
        lambdaQueryWrapper.eq(CharSequenceUtil.isNotEmpty(storeFlowQueryDTO.getType()),
                StoreFlow::getFlowType, storeFlowQueryDTO.getType());

        //售后编号判定
        lambdaQueryWrapper.eq(CharSequenceUtil.isNotEmpty(storeFlowQueryDTO.getRefundSn()),
                StoreFlow::getRefundSn, storeFlowQueryDTO.getRefundSn());

        //售后编号判定
        lambdaQueryWrapper.eq(CharSequenceUtil.isNotEmpty(storeFlowQueryDTO.getOrderSn()),
                StoreFlow::getOrderSn, storeFlowQueryDTO.getOrderSn());

        //结算单非空，则校对结算单参数
        if (storeFlowQueryDTO.getBill() != null) {
            Bill bill = storeFlowQueryDTO.getBill();
            lambdaQueryWrapper.eq(CharSequenceUtil.isNotEmpty(bill.getStoreId()), StoreFlow::getStoreId, bill.getStoreId());
            lambdaQueryWrapper.between(bill.getStartTime() != null && bill.getEndTime() != null,
                    StoreFlow::getCreateTime, bill.getStartTime(), bill.getEndTime());
        }
        return lambdaQueryWrapper;
    }
}