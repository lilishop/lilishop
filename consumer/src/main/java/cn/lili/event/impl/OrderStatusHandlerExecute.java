package cn.lili.event.impl;

import cn.lili.event.AfterSaleStatusChangeEvent;
import cn.lili.event.TradeEvent;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.service.TradeService;
import cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 订单状态处理类
 *
 * @author Chopper
 * @since 2020-07-03 11:20
 **/
@Service
public class OrderStatusHandlerExecute implements TradeEvent, AfterSaleStatusChangeEvent {


    @Autowired
    private TradeService tradeService;
    @Autowired
    private OrderItemService orderItemService;
    @Autowired
    private OrderService orderService;

    @Override
    public void orderCreate(TradeDTO tradeDTO) {
        //如果订单需要支付金额为0，则将订单步入到下一个流程
        if (tradeDTO.getPriceDetailDTO().getFlowPrice() <= 0) {
            tradeService.payTrade(tradeDTO.getSn(), PaymentMethodEnum.BANK_TRANSFER.name(), "-1");
        }

    }

    @Override
    public void afterSaleStatusChange(AfterSale afterSale) {
        if (afterSale.getServiceStatus().equals(AfterSaleStatusEnum.COMPLETE.name())) {
            //循环订单货物，判断是否已经全部售后
            List<OrderItem> orderItems = orderItemService.getByOrderSn(afterSale.getOrderSn());
            // 总退货数量
            int returnCount = 0;
            // 总购买数量
            int deliverCount = 0;
            for (OrderItem orderItem : orderItems) {
                returnCount += orderItem.getReturnGoodsNumber();
                deliverCount += orderItem.getNum();
            }
            if (returnCount == deliverCount) {
                orderService.systemCancel(afterSale.getOrderSn(),"订单货物全部退款",false);
            }

        }
    }
}
