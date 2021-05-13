package cn.lili.event.impl;

import cn.hutool.core.date.DateTime;
import cn.lili.event.AfterSaleStatusChangeEvent;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.entity.enums.DistributionOrderStatusEnum;
import cn.lili.modules.distribution.mapper.DistributionOrderMapper;
import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.order.order.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum;
import cn.lili.timetask.handler.EveryDayExecute;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 分销订单入库
 *
 * @author Chopper
 * @date 2020-07-03 11:20
 */
@Service
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class DistributionOrderExecute implements OrderStatusChangeEvent, EveryDayExecute, AfterSaleStatusChangeEvent {

    //分销订单
    private final DistributionOrderService distributionOrderService;
    //分销订单持久层
    private final DistributionOrderMapper distributionOrderMapper;


    @Override
    public void orderChange(OrderMessage orderMessage) {

        switch (orderMessage.getNewStatus()) {
            case PAID: {
                //记录分销订单
                distributionOrderService.payOrder(orderMessage.getOrderSn());
                break;
            }
            case CANCELLED: {
                //修改分销订单状态
                distributionOrderService.cancelOrder(orderMessage.getOrderSn());
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
