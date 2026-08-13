package cn.lili.modules.order.order.entity.vo;

import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.DeliverStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 商家发货操作权限单元测试
 */
class AllowOperationDeliveryTest {

    @Test
    void undeliveredNormalOrderAllowsWholeSplitAndNoDelivery() {
        Order order = baseNormalOrder();
        order.setOrderStatus(OrderStatusEnum.UNDELIVERED.name());
        order.setDeliverStatus(DeliverStatusEnum.UNDELIVERED.name());

        AllowOperation allowOperation = new AllowOperation(order);

        assertThat(allowOperation.getShip()).isTrue();
        assertThat(allowOperation.getFulfillWhole()).isTrue();
        assertThat(allowOperation.getFulfillSplit()).isTrue();
        assertThat(allowOperation.getFulfillNoDelivery()).isTrue();
        assertThat(allowOperation.getShowLogistics()).isFalse();
    }

    @Test
    void partsDeliveredOrderOnlyAllowsSplitOnDetailAndList() {
        Order order = baseNormalOrder();
        order.setOrderStatus(OrderStatusEnum.PARTS_DELIVERED.name());
        order.setDeliverStatus(DeliverStatusEnum.PARTS_DELIVERED.name());

        AllowOperation detailOperation = new AllowOperation(order);
        assertThat(detailOperation.getShip()).isTrue();
        assertThat(detailOperation.getFulfillWhole()).isFalse();
        assertThat(detailOperation.getFulfillSplit()).isTrue();
        assertThat(detailOperation.getFulfillNoDelivery()).isFalse();
        assertThat(detailOperation.getShowLogistics()).isTrue();

        OrderSimpleVO simpleVO = new OrderSimpleVO();
        simpleVO.setOrderType(OrderTypeEnum.NORMAL.name());
        simpleVO.setOrderStatus(OrderStatusEnum.PARTS_DELIVERED.name());
        simpleVO.setDeliverStatus(DeliverStatusEnum.PARTS_DELIVERED.name());
        simpleVO.setPayStatus(PayStatusEnum.PAID.name());

        AllowOperation listOperation = new AllowOperation(simpleVO);
        assertThat(listOperation.getShip()).isTrue();
        assertThat(listOperation.getFulfillWhole()).isFalse();
        assertThat(listOperation.getFulfillSplit()).isTrue();
        assertThat(listOperation.getFulfillNoDelivery()).isFalse();
        assertThat(listOperation.getShowLogistics()).isTrue();
    }

    private Order baseNormalOrder() {
        Order order = new Order();
        order.setOrderType(OrderTypeEnum.NORMAL.name());
        order.setDeliveryMethod(DeliveryMethodEnum.LOGISTICS.name());
        order.setPayStatus(PayStatusEnum.PAID.name());
        return order;
    }
}
