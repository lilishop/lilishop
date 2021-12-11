package cn.lili.event.impl;

import cn.hutool.core.date.DateTime;
import cn.lili.event.AfterSaleStatusChangeEvent;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.entity.enums.DistributionOrderStatusEnum;
import cn.lili.modules.distribution.mapper.DistributionOrderMapper;
import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum;
import cn.lili.timetask.handler.EveryDayExecute;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * 分销订单入库
 *
 * @author Chopper
 * @since 2020-07-03 11:20
 */
@Slf4j
@Service
public class DistributionOrderExecute implements OrderStatusChangeEvent, EveryDayExecute, AfterSaleStatusChangeEvent {

    /**
     * 分销订单
     */
    @Autowired
    private DistributionOrderService distributionOrderService;
    /**
     * 分销订单持久层
     */
    @Resource
    private DistributionOrderMapper distributionOrderMapper;


    @Override
    public void orderChange(OrderMessage orderMessage) {

        switch (orderMessage.getNewStatus()) {
            //订单带校验/订单代发货，则记录分销信息
            case TAKE:
            case UNDELIVERED: {
                //记录分销订单
                distributionOrderService.calculationDistribution(orderMessage.getOrderSn());
                break;
            }
            case CANCELLED: {
                //修改分销订单状态
                distributionOrderService.cancelOrder(orderMessage.getOrderSn());
                break;
            }
            default: {
                break;
            }
        }
    }

    @Override
    public void execute() {
        //计算分销提佣
        distributionOrderMapper.rebate(DistributionOrderStatusEnum.WAIT_BILL.name(), new DateTime());

        //修改分销订单状态
        distributionOrderService.update(new LambdaUpdateWrapper<DistributionOrder>()
                .eq(DistributionOrder::getDistributionOrderStatus, DistributionOrderStatusEnum.WAIT_BILL.name())
                .le(DistributionOrder::getSettleCycle, new DateTime())
                .set(DistributionOrder::getDistributionOrderStatus, DistributionOrderStatusEnum.WAIT_CASH.name()));

    }

    @Override
    public void afterSaleStatusChange(AfterSale afterSale) {
        if (afterSale.getServiceStatus().equals(AfterSaleStatusEnum.COMPLETE.name())) {
            distributionOrderService.refundOrder(afterSale.getSn());
        }
    }

}
