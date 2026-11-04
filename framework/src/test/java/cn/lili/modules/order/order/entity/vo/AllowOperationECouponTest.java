package cn.lili.modules.order.order.entity.vo;

import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * E_COUPON 订单操作权限：无物流/核销/改价。
 *
 * @author Mike
 * @date 2026-08-02
 */
class AllowOperationECouponTest {

    /**
     * 验证已完成卡密订单禁用发货、确认收货、物流及核销操作。
     */
    @Test
    void completedECouponDisablesShipRogLogisticsAndTake() {
        Order order = eCouponOrder(OrderStatusEnum.COMPLETED.name(), PayStatusEnum.PAID.name());

        AllowOperation ops = new AllowOperation(order);

        assertThat(ops.getShip()).isFalse();
        assertThat(ops.getRog()).isFalse();
        assertThat(ops.getShowLogistics()).isFalse();
        assertThat(ops.getTake()).isFalse();
        assertThat(ops.getEditConsignee()).isFalse();
        assertThat(ops.getEditPrice()).isFalse();
    }

    /**
     * 验证未付款卡密订单仅允许取消及付款。
     */
    @Test
    void unpaidECouponAllowsCancelAndPayOnly() {
        Order order = eCouponOrder(OrderStatusEnum.UNPAID.name(), PayStatusEnum.UNPAID.name());

        AllowOperation ops = new AllowOperation(order);

        assertThat(ops.getCancel()).isTrue();
        assertThat(ops.getPay()).isTrue();
        assertThat(ops.getShip()).isFalse();
    }

    /**
     * 构造卡密订单测试数据。
     *
     * @param orderStatus 订单状态
     * @param payStatus   付款状态
     * @return 卡密订单测试数据
     */
    private Order eCouponOrder(String orderStatus, String payStatus) {
        Order order = new Order();
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderStatus(orderStatus);
        order.setPayStatus(payStatus);
        return order;
    }
}
