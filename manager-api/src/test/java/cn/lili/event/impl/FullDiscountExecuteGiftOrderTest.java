package cn.lili.event.impl;

import cn.hutool.core.lang.Snowflake;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.utils.SnowFlake;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.service.OrderLogService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 满赠 E_COUPON 赠品子单创建与落库归一化。
 *
 * @author Mike
 * @date 2026-08-02
 */
class FullDiscountExecuteGiftOrderTest {

    private FullDiscountExecute execute;
    private OrderService orderService;

    @BeforeEach
    void setUp() {
        execute = new FullDiscountExecute();
        orderService = mock(OrderService.class);
        OrderItemService orderItemService = mock(OrderItemService.class);
        OrderLogService orderLogService = mock(OrderLogService.class);
        LiliMessageTemplate messageTemplate = mock(LiliMessageTemplate.class);
        RocketmqCustomProperties rocketmqCustomProperties = mock(RocketmqCustomProperties.class);

        ReflectionTestUtils.setField(execute, "orderService", orderService);
        ReflectionTestUtils.setField(execute, "orderItemService", orderItemService);
        ReflectionTestUtils.setField(execute, "orderLogService", orderLogService);
        ReflectionTestUtils.setField(execute, "liliMessageTemplate", messageTemplate);
        ReflectionTestUtils.setField(execute, "rocketmqCustomProperties", rocketmqCustomProperties);
        when(rocketmqCustomProperties.getOrderTopic()).thenReturn("order-topic");
        when(orderService.queryListByParams(any())).thenReturn(Collections.emptyList());
        ReflectionTestUtils.setField(SnowFlake.class, "snowflake", new Snowflake(1, 1));
    }

    @Test
    void giftOrderHandlerNormalizesECouponSubOrder() {
        Order origin = originOrder();
        GoodsSku giftSku = giftSku();

        ReflectionTestUtils.invokeMethod(execute, "giftOrderHandler",
                List.of(giftSku), origin, OrderTypeEnum.E_COUPON);

        ArgumentCaptor<Order> captor = ArgumentCaptor.forClass(Order.class);
        verify(orderService).save(captor.capture());
        Order saved = captor.getValue();
        assertThat(saved.getOrderType()).isEqualTo(OrderTypeEnum.E_COUPON.name());
        assertThat(saved.getOrderPromotionType()).isEqualTo(OrderPromotionTypeEnum.GIFT.name());
        assertThat(saved.getOrderStatus()).isEqualTo(cn.lili.modules.order.order.entity.enums.OrderStatusEnum.PAID.name());
        assertThat(saved.getPayStatus()).isEqualTo(cn.lili.modules.order.order.entity.enums.PayStatusEnum.PAID.name());
        assertThat(saved.getDeliveryMethod()).isEqualTo(DeliveryMethodEnum.VIRTUAL.name());
        assertThat(saved.getFreightPrice()).isZero();
        assertThat(saved.getConsigneeName()).isNull();
        assertThat(saved.getConsigneeDetail()).isNull();
    }

    @Test
    void giftOrderHandlerSkipsWhenGiftSubOrderAlreadyExists() {
        Order origin = originOrder();
        GoodsSku giftSku = giftSku();
        when(orderService.queryListByParams(any())).thenReturn(List.of(new Order()));

        ReflectionTestUtils.invokeMethod(execute, "giftOrderHandler",
                List.of(giftSku), origin, OrderTypeEnum.E_COUPON);

        verify(orderService, never()).save(any());
    }

    private Order originOrder() {
        Order order = new Order();
        order.setSn("P100");
        order.setMemberId("m1");
        order.setMemberName("buyer");
        order.setClientType("H5");
        order.setDeliveryMethod(DeliveryMethodEnum.LOGISTICS.name());
        order.setConsigneeName("张三");
        order.setConsigneeMobile("13800000000");
        order.setConsigneeDetail("测试地址");
        order.setFreightPrice(10D);
        return order;
    }

    private GoodsSku giftSku() {
        GoodsSku sku = new GoodsSku();
        sku.setId("sku-gift");
        sku.setGoodsName("赠品卡密");
        sku.setThumbnail("img.png");
        sku.setPrice(0D);
        sku.setGoodsType(GoodsTypeEnum.E_COUPON.name());
        sku.setCategoryPath("1,2,cat-1");
        return sku;
    }
}
