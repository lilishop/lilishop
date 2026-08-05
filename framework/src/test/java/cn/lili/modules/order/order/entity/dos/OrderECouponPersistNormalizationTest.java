package cn.lili.modules.order.order.entity.dos;

import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * E_COUPON 订单落库归一化单元测试。
 *
 * @author Mike
 * @date 2026-08-02
 */
class OrderECouponPersistNormalizationTest {

    @Test
    void applyECouponPersistNormalization_clearsLogisticsFields() {
        Order order = new Order();
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setDeliveryMethod(DeliveryMethodEnum.LOGISTICS.name());
        order.setFreightPrice(12D);
        order.setConsigneeName("张三");
        order.setConsigneeMobile("13800000000");
        order.setConsigneeAddressPath("省,市,区");
        order.setConsigneeAddressIdPath("1,2,3");
        order.setConsigneeDetail("详细地址");
        order.setStoreAddressPath("自提点");
        order.setLogisticsNo("SF123");

        order.applyECouponPersistNormalization();

        assertThat(order.getDeliveryMethod()).isEqualTo(DeliveryMethodEnum.VIRTUAL.name());
        assertThat(order.getFreightPrice()).isZero();
        assertThat(order.getConsigneeName()).isNull();
        assertThat(order.getConsigneeMobile()).isNull();
        assertThat(order.getConsigneeAddressPath()).isNull();
        assertThat(order.getConsigneeAddressIdPath()).isNull();
        assertThat(order.getConsigneeDetail()).isNull();
        assertThat(order.getStoreAddressPath()).isNull();
        assertThat(order.getLogisticsNo()).isNull();
    }

    @Test
    void applyECouponPersistNormalization_skipsNonECoupon() {
        Order order = new Order();
        order.setOrderType(OrderTypeEnum.NORMAL.name());
        order.setDeliveryMethod(DeliveryMethodEnum.LOGISTICS.name());
        order.setConsigneeName("张三");

        order.applyECouponPersistNormalization();

        assertThat(order.getDeliveryMethod()).isEqualTo(DeliveryMethodEnum.LOGISTICS.name());
        assertThat(order.getConsigneeName()).isEqualTo("张三");
    }
}
