package cn.lili.event.impl;

import cn.lili.event.AfterSaleStatusChangeEvent;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.distribution.service.DistributionRewardService;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 分销订单入库
 *
 * @author Chopper
 * @since 2020-07-03 11:20
 */
@Service
public class DistributionOrderExecute implements OrderStatusChangeEvent, AfterSaleStatusChangeEvent {

    /**
     * 分销订单
     */
    @Autowired
    private DistributionOrderService distributionOrderService;

    /**
     * 分销奖励（判断 afterCommit 是否已入账）。
     */
    @Autowired
    private DistributionRewardService distributionRewardService;

    /**
     * 订单状态变更时处理分销入账、补偿与取消。
     *
     * @param orderMessage 订单状态消息
     */
    @Override
    public void orderChange(OrderMessage orderMessage) {
        String orderSn = orderMessage.getOrderSn();
        switch (orderMessage.getNewStatus()) {
            case PAID:
            case TAKE:
            case STAY_PICKED_UP:
            case UNDELIVERED: {
                // 已有分销单则跳过，避免与 afterCommit 双写
                if (!hasRewardRecords(orderSn)) {
                    // afterCommit 未写入时由 MQ 补偿入账
                    distributionOrderService.handlePaymentRewards(orderSn);
                }
                break;
            }
            case CANCELLED: {
                distributionOrderService.cancelOrder(orderMessage.getOrderSn());
                break;
            }
            default: {
                break;
            }
        }
    }

    /**
     * 判断订单是否已生成 V2 奖励记录。
     *
     * @param orderSn 订单编号
     * @return 是否已有奖励记录
     */
    private boolean hasRewardRecords(String orderSn) {
        return distributionRewardService.hasRewardsByOrderSn(orderSn);
    }

    /**
     * 售后完成时触发分销退款冲回。
     *
     * @param afterSale 售后单
     */
    @Override
    public void afterSaleStatusChange(AfterSale afterSale) {
        if (afterSale.getServiceStatus().equals(AfterSaleStatusEnum.COMPLETE.name())) {
            distributionOrderService.refundOrder(afterSale);
        }
    }

}
