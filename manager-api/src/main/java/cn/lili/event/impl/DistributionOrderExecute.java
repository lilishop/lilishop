package cn.lili.event.impl;

import cn.hutool.core.date.DateField;
import cn.hutool.core.date.DateTime;
import cn.hutool.json.JSONUtil;
import cn.lili.event.AfterSaleStatusChangeEvent;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.distribution.entity.enums.DistributionOrderStatusEnum;
import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.DistributionSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.timetask.handler.EveryDayExecute;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 分销订单入库
 *
 * @author Chopper
 * @since 2020-07-03 11:20
 */
@Slf4j
@Service
public class DistributionOrderExecute implements OrderStatusChangeEvent, AfterSaleStatusChangeEvent {

    /**
     * 分销订单
     */
    @Autowired
    private DistributionOrderService distributionOrderService;

    @Override
    public void orderChange(OrderMessage orderMessage) {

        switch (orderMessage.getNewStatus()) {
            //订单带校验/订单代发货/待自提，则记录分销信息
            case TAKE:
            case STAY_PICKED_UP:
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
    public void afterSaleStatusChange(AfterSale afterSale) {
        if (afterSale.getServiceStatus().equals(AfterSaleStatusEnum.COMPLETE.name())) {
            distributionOrderService.refundOrder(afterSale);
        }
    }

}
