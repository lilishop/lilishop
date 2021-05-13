package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.core.date.DateField;
import cn.hutool.core.date.DateTime;
import cn.hutool.json.JSONUtil;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.PageUtil;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.entity.enums.DistributionOrderStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionOrderSearchParams;
import cn.lili.modules.distribution.mapper.DistributionOrderMapper;
import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.service.StoreFlowService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.DistributionSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;


/**
 * 分销订单接口实现
 *
 * @author pikachu
 * @date 2020-03-14 23:04:56
 */
@Service
@Transactional
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class DistributionOrderServiceImpl extends ServiceImpl<DistributionOrderMapper, DistributionOrder> implements DistributionOrderService {

    //订单
    private final OrderService orderService;
    //店铺流水
    private final StoreFlowService storeFlowService;
    //分销员
    private final DistributionService distributionService;
    //系统设置
    private final SettingService settingService;

    @Override
    public IPage<DistributionOrder> getDistributionOrderPage(DistributionOrderSearchParams distributionOrderSearchParams) {
        return this.page(PageUtil.initPage(distributionOrderSearchParams), distributionOrderSearchParams.queryWrapper());

    }

    @Override
    public void payOrder(String orderSn) {

        //根据订单编号获取订单数据
        Order order = orderService.getBySn(orderSn);

        //判断是否为分销订单，如果为分销订单则获取分销佣金
        if (order.getDistributionId() != null) {

            //根据订单编号获取有分销金额的店铺流水记录
            List<StoreFlow> storeFlowList = storeFlowService.list(new LambdaQueryWrapper<StoreFlow>()
                    .eq(StoreFlow::getOrderSn, orderSn)
                    .isNotNull(StoreFlow::getDistributionRebate));
            for (StoreFlow storeFlow : storeFlowList) {
                DistributionOrder distributionOrder = new DistributionOrder(storeFlow);
                distributionOrder.setDistributionId(order.getDistributionId());

                //分销员信息
                Distribution distribution = distributionService.getById(order.getDistributionId());
                distributionOrder.setDistributionName(distribution.getMemberName());

                //设置结算天数(解冻日期)
                Setting setting = settingService.get(SettingEnum.DISTRIBUTION_SETTING.name());
                DistributionSetting distributionSetting = JSONUtil.toBean(setting.getSettingValue(), DistributionSetting.class);
                DateTime dateTime = new DateTime();
                // dateTime.offsetNew(DateField.DAY_OF_MONTH,distributionSetting.getCashDay());
                dateTime.offsetNew(DateField.DAY_OF_MONTH, 1);
                distributionOrder.setSettleCycle(dateTime);
                this.save(distributionOrder);
            }
        }
    }

    @Override
    public void cancelOrder(String orderSn) {
        this.update(new LambdaUpdateWrapper<DistributionOrder>().eq(DistributionOrder::getOrderSn, orderSn)
                .set(DistributionOrder::getDistributionOrderStatus, DistributionOrderStatusEnum.CANCEL.name()));
    }

    @Override
    public void refundOrder(String afterSaleSn) {
        //判断是否为分销订单
        StoreFlow storeFlow = storeFlowService.getOne(new LambdaQueryWrapper<StoreFlow>()
                .eq(StoreFlow::getRefundSn, afterSaleSn)
                .isNotNull(StoreFlow::getDistributionRebate));
        if (storeFlow != null) {

            //获取收款分销订单
            DistributionOrder distributionOrder = this.getOne(new LambdaQueryWrapper<DistributionOrder>()
                    .eq(DistributionOrder::getOrderItemSn, storeFlow.getOrderItemSn()));

            //已提交无法重复提交
            //如果未结算则将分销订单取消
            //如果已结算则创建退款分销订单
            if(distributionOrder.getDistributionOrderStatus().equals(DistributionOrderStatusEnum.CANCEL.name())){
                return ;
            } else if(distributionOrder.getDistributionOrderStatus().equals(DistributionOrderStatusEnum.WAIT_BILL.name())){
                this.update(new LambdaUpdateWrapper<DistributionOrder>().eq(DistributionOrder::getOrderItemSn, storeFlow.getOrderItemSn())
                        .set(DistributionOrder::getDistributionOrderStatus, DistributionOrderStatusEnum.CANCEL.name()));
            }else{
                //创建分销退款订单
                DistributionOrder backDistributionOrder = new DistributionOrder();

                this.save(backDistributionOrder);
                //修改分销员提成金额
                distributionService.updateCanRebate(CurrencyUtil.sub(0,storeFlow.getDistributionRebate()),distributionOrder.getDistributionId());
            }
        }
    }

}