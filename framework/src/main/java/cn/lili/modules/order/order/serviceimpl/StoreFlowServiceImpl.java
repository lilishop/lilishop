package cn.lili.modules.order.order.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.order.order.entity.dto.StoreFlowProfitSharingDTO;
import cn.lili.modules.order.order.entity.dto.StoreFlowQueryDTO;
import cn.lili.modules.order.order.entity.enums.FlowTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.enums.ProfitSharingStatusEnum;
import cn.lili.modules.order.order.mapper.StoreFlowMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.service.StoreFlowService;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.service.RefundLogService;
import cn.lili.modules.store.entity.dos.Bill;
import cn.lili.modules.store.entity.dto.BillSearchParams;
import cn.lili.modules.store.entity.vos.StoreFlowPayDownloadVO;
import cn.lili.modules.store.entity.vos.StoreFlowRefundDownloadVO;
import cn.lili.modules.store.service.BillService;
import cn.lili.modules.system.entity.dto.payment.WechatPaymentSetting;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Date;
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

    @Autowired
    private DistributionOrderService distributionOrderService;

    /**
     * 店铺订单支付流水
     *
     * @param orderSn 订单编号
     */
    @Override
    public void payOrder(String orderSn) {
        //根据订单编号获取子订单列表
        List<OrderItem> orderItems = orderItemService.getByOrderSn(orderSn);
        //根据订单编号获取订单数据
        Order order = orderService.getBySn(orderSn);

        //循环子订单记录流水
        for (OrderItem item : orderItems) {
            StoreFlow storeFlow = new StoreFlow(order, item, FlowTypeEnum.PAY);
            saveProfitSharing(storeFlow);

            //添加付款交易流水
            this.save(storeFlow);
        }
    }

    @Override
    public void orderCancel(String orderSn) {
        //根据订单编号获取订单数据
        Order order = orderService.getBySn(orderSn);
        // 判断订单是否是付款
        if (!PayStatusEnum.PAID.name()
                .equals((order.getPayStatus()))) {
            return;
        }
        List<OrderItem> items = orderItemService.getByOrderSn(order.getSn());
        List<StoreFlow> storeFlows = new ArrayList<>();

        //修改付款记录
        this.update(new LambdaUpdateWrapper<StoreFlow>()
                .eq(StoreFlow::getOrderSn, order.getSn())
                .set(StoreFlow::getBillTime, new Date())
                .set(StoreFlow::getProfitSharingStatus, ProfitSharingStatusEnum.ORDER_CANCEL.name())
                .set(StoreFlow::getFullRefund, true));

        //记录退款记录
        for (OrderItem item : items) {
            StoreFlow storeFlow = new StoreFlow(order, item, FlowTypeEnum.REFUND);
            storeFlow.setProfitSharingStatus(ProfitSharingStatusEnum.ORDER_CANCEL.name());
            storeFlow.setBillTime(new Date());
            storeFlows.add(storeFlow);
        }
        this.saveBatch(storeFlows);
    }


    /**
     * 店铺订单退款流水
     *
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
        //流水金额 = 支付最终结算金额
        storeFlow.setFinalPrice(afterSale.getActualRefundPrice());
        //站点优惠券补贴返还金额=(站点优惠券补贴金额/购买商品数量)*退款商品数量
        storeFlow.setSiteCouponCommission(CurrencyUtil.mul(CurrencyUtil.div(payStoreFlow.getSiteCouponCommission() == null ? 0 : payStoreFlow.getSiteCouponCommission(), payStoreFlow.getNum()), afterSale.getNum()));
        //平台优惠券 使用金额
        storeFlow.setSiteCouponPrice(payStoreFlow.getSiteCouponPrice());
        //站点优惠券佣金比例
        storeFlow.setSiteCouponPoint(payStoreFlow.getSiteCouponPoint());

        // 退单结算金额 相当于付款结算金额反计算逻辑，对平台收取的佣金，分销收取的佣金进行返还，对平台优惠券的补贴应该相加
        // 由于退单的结算金额为正数，所以需要将结算金额计算方式，相较于付款结算金额计算方式进行反转

        // 退款结算金额 = flowPrice(实际退款金额) + storeCouponCommission(getSiteCouponCommission) - platFormCommission(平台收取交易佣金) - distributionCommission(单品分销返现支出) ")

        storeFlow.setBillPrice(
                CurrencyUtil.add(
                        afterSale.getActualRefundPrice(),
                        storeFlow.getSiteCouponCommission(),
                        -storeFlow.getCommissionPrice(),
                        -storeFlow.getDistributionRebate()
                )
        );
        //退款日志
        RefundLog refundLog = refundLogService.queryByAfterSaleSn(afterSale.getSn());
        //第三方流水单号
        storeFlow.setTransactionId(refundLog.getReceivableNo());
        //支付方式
        storeFlow.setPaymentName(refundLog.getPaymentName());



        //修改付款的StoreFlow
        StoreFlowProfitSharingDTO storeFlowProfitSharingDTO = JSONUtil.toBean(payStoreFlow.getProfitSharing(), StoreFlowProfitSharingDTO.class);
        if (storeFlow.getBillPrice()
                .equals(payStoreFlow.getFinalPrice())) {
            payStoreFlow.setFullRefund(true);
            storeFlowProfitSharingDTO.setPrice(0D);
            storeFlowProfitSharingDTO.setStorePrice(0D);
            storeFlowProfitSharingDTO.setPlatformPrice(0D);
            storeFlowProfitSharingDTO.setDistributionPrice(0D);
            storeFlowProfitSharingDTO.setSubsidies(0D);
            payStoreFlow.setBillTime(new Date());
            payStoreFlow.setProfitSharingStatus(ProfitSharingStatusEnum.ORDER_CANCEL.name());
            //设置退款时间
            storeFlow.setBillTime(new Date());
        } else {
            //计算 累计订单退款金额，修改分账信息
            Integer refundNum = this.baseMapper.getRefundNum(payStoreFlow.getOrderItemSn());
            refundNum = refundNum == null ? 0 : refundNum;
            int allNum = storeFlow.getNum() + refundNum;
            Double proportion = CurrencyUtil.div((payStoreFlow.getNum() - allNum), payStoreFlow.getNum());
            if (proportion.equals(0D)) {
                payStoreFlow.setFullRefund(true);
                storeFlowProfitSharingDTO.setPrice(0D);
                storeFlowProfitSharingDTO.setStorePrice(0D);
                storeFlowProfitSharingDTO.setPlatformPrice(0D);
                storeFlowProfitSharingDTO.setDistributionPrice(0D);
                storeFlowProfitSharingDTO.setSubsidies(0D);
                payStoreFlow.setBillTime(new Date());
                payStoreFlow.setProfitSharingStatus(ProfitSharingStatusEnum.ORDER_CANCEL.name());
                //设置退款时间
                storeFlow.setBillTime(new Date());
            } else {
                storeFlowProfitSharingDTO.setPrice(CurrencyUtil.mul(payStoreFlow.getFinalPrice(), proportion));
                storeFlowProfitSharingDTO.setStorePrice(CurrencyUtil.mul(payStoreFlow.getBillPrice(), proportion));
                storeFlowProfitSharingDTO.setPlatformPrice(CurrencyUtil.mul(payStoreFlow.getCommissionPrice(), proportion));
                storeFlowProfitSharingDTO.setSubsidies(CurrencyUtil.mul(payStoreFlow.getSiteCouponCommission(), proportion));
                storeFlowProfitSharingDTO.setDistributionPrice(CurrencyUtil.mul(payStoreFlow.getDistributionRebate(), proportion));
            }
        }
        payStoreFlow.setProfitSharing(JSONUtil.toJsonStr(storeFlowProfitSharingDTO));
        //修改付款流水
        this.updateById(payStoreFlow);
        //保存退款流水·
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

    @Override
    public void updateProfitSharingStatus() {
        //获取已完成的列表，进行相关的处理
        List<StoreFlow> storeFlowList = this.baseMapper.completeList();

        for (StoreFlow storeFlow : storeFlowList) {
            distributionOrderService.completeOrder(storeFlow);
        }
        this.baseMapper.updateProfitSharingStatus();
    }

    @Override
    public Bill getRefundBill(BillSearchParams searchParams) {
        return this.baseMapper.getRefundBill(searchParams.queryWrapper());
    }

    @Override
    public Bill getOrderBill(BillSearchParams searchParams) {
        return this.baseMapper.getOrderBill(searchParams.queryWrapper());
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

        //订单编号判定
        lambdaQueryWrapper.eq(CharSequenceUtil.isNotEmpty(storeFlowQueryDTO.getOrderSn()),
                StoreFlow::getOrderSn, storeFlowQueryDTO.getOrderSn());

        //订单货物编号
        lambdaQueryWrapper.eq(CharSequenceUtil.isNotEmpty(storeFlowQueryDTO.getOrderItemSn()),
                StoreFlow::getOrderItemSn, storeFlowQueryDTO.getOrderItemSn());
        //结算单非空，则校对结算单参数
        if (storeFlowQueryDTO.getBill() != null) {
            Bill bill = storeFlowQueryDTO.getBill();
            lambdaQueryWrapper.eq(CharSequenceUtil.isNotEmpty(bill.getStoreId()), StoreFlow::getStoreId, bill.getStoreId());
            lambdaQueryWrapper.ge(bill.getStartTime() != null && bill.getEndTime() != null,
                    StoreFlow::getBillTime, bill.getStartTime());
            lambdaQueryWrapper.lt(bill.getStartTime() != null && bill.getEndTime() != null,
                    StoreFlow::getBillTime, bill.getEndTime());
        }
        return lambdaQueryWrapper;
    }


    /**
     * 添加分账内容
     *
     * @param storeFlow 店铺流水
     */
    private void saveProfitSharing(StoreFlow storeFlow) {

        StoreFlowProfitSharingDTO storeFlowProfitSharingDTO = new StoreFlowProfitSharingDTO();
        //店铺获取
        storeFlowProfitSharingDTO.setStorePrice(storeFlow.getBillPrice());
        //平台佣金
        storeFlowProfitSharingDTO.setPlatformPrice(storeFlow.getCommissionPrice());
        //分销佣金
        storeFlowProfitSharingDTO.setDistributionPrice(storeFlow.getDistributionRebate());
        //总金额
        storeFlowProfitSharingDTO.setPrice(storeFlow.getFinalPrice());
        //平台补差
        storeFlowProfitSharingDTO.setSubsidies(storeFlow.getSiteCouponCommission());
        //分账详情
        storeFlow.setProfitSharing(JSONUtil.toJsonStr(storeFlowProfitSharingDTO));
        //分账状态
        storeFlow.setProfitSharingStatus(ProfitSharingStatusEnum.WAIT_COMPLETE.name());

    }
}