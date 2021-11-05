package cn.lili.buyer.test.cart;

import cn.lili.event.impl.StockUpdateExecute;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

/**
 * 订单库存扣减
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest
class CartTest {

    @Autowired
    private StockUpdateExecute stockUpdateExecute;

    //订单支付，库存扣减单元测试
    @Test
    void stockUpdate() {
        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setOrderSn("O202102221363668802717351937");//订单sn
        orderMessage.setNewStatus(OrderStatusEnum.PAID);
        orderMessage.setPaymentMethod(PaymentMethodEnum.WALLET.name());
        stockUpdateExecute.orderChange(orderMessage);
    }


}
