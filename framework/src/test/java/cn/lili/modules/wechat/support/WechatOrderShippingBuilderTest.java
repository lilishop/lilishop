package cn.lili.modules.wechat.support;

import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dos.OrderPackage;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderPackageService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * 验证合并支付完整履约、配送类型、物流必填和敏感联系方式掩码的真实边界。
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
class WechatOrderShippingBuilderTest {
    private WechatOrderShippingBuilder builder;
    private OrderPackageService packages;

    /** 使用内存商品与包裹依赖，不连接数据库及微信。 */
    @BeforeEach
    void setUp() {
        builder = new WechatOrderShippingBuilder();
        packages = mock(OrderPackageService.class);
        OrderItemService items = mock(OrderItemService.class);
        OrderItem item = new OrderItem(); item.setGoodsName("测试商品");
        when(items.getByOrderSn(anyString())).thenReturn(List.of(item));
        when(packages.orderPackageList(anyString())).thenReturn(List.of());
        ReflectionTestUtils.setField(builder, "itemService", items);
        ReflectionTestUtils.setField(builder, "packageService", packages);
    }

    /** 多店铺同笔支付全部发货后包含所有包裹，稳定排序且明确全部发货。 */
    @Test
    void mergedPaymentIncludesEveryPackageAndMasksMobile() {
        Order a = order("A", "DELIVERED", "NORMAL");
        Order b = order("B", "DELIVERED", "NORMAL");
        Map<String,Object> request = builder.build(List.of(b,a), name -> "SF");
        assertEquals(2, request.get("delivery_mode"));
        assertEquals(true, request.get("is_all_delivered"));
        List<?> rows = (List<?>) request.get("shipping_list");
        assertEquals(2, rows.size());
        assertEquals("tracking-A", ((Map<?,?>) rows.get(0)).get("tracking_no"));
        assertEquals("****5678", ((Map<?,?>)((Map<?,?>) rows.get(0)).get("contact")).get("receiver_contact"));
    }

    /** 仍有未发货子订单时不产生可上报的部分快照。 */
    @Test
    void mergedPaymentWaitsForAllChildren() {
        WechatOrderSyncException e = assertThrows(WechatOrderSyncException.class,
                () -> builder.build(List.of(order("A","DELIVERED","NORMAL"),order("B","PARTS_DELIVERED","NORMAL")), name -> "SF"));
        assertTrue(e.isPending());
        assertEquals("WAIT_SHIPPING", e.getCode());
    }

    /** 混合快递与虚拟配送必须核查，禁止用任一子订单替代全部履约。 */
    @Test
    void mergedMixedDeliveryIsExplicitlyRejected() {
        WechatOrderSyncException e = assertThrows(WechatOrderSyncException.class,
                () -> builder.build(List.of(order("A","DELIVERED","NORMAL"),order("B","TAKE","VIRTUAL")), name -> "SF"));
        assertEquals("MIXED_DELIVERY", e.getCode());
        assertFalse(e.isPending());
    }

    /** 虚拟和电子卡券仅在已交付节点上报，不在支付后直接上报。 */
    @Test
    void virtualAndCouponRequireDelivery() {
        assertEquals(3, builder.build(List.of(order("A","TAKE","VIRTUAL")), name -> "SF").get("logistics_type"));
        assertEquals(3, builder.build(List.of(order("B","COMPLETED","E_COUPON")), name -> "SF").get("logistics_type"));
        assertTrue(assertThrows(WechatOrderSyncException.class,
                () -> builder.build(List.of(order("C","PAID","E_COUPON")), name -> "SF")).isPending());
    }

    /** 预约服务只能在完成后上报为虚拟交付，不伪造快递。 */
    @Test
    void appointmentWaitsForCompletion() {
        assertTrue(assertThrows(WechatOrderSyncException.class,
                () -> builder.build(List.of(order("A","PAID","APPOINTMENT")), name -> "SF")).isPending());
        assertEquals(3, builder.build(List.of(order("A","COMPLETED","APPOINTMENT")), name -> "SF").get("logistics_type"));
    }

    /** 已备货自提无需快递单号，映射自提类型。 */
    @Test
    void selfPickupUsesActualDeliveryType() {
        Order pickup = order("A","STAY_PICKED_UP","NORMAL"); pickup.setDeliveryMethod("SELF_PICK_UP"); pickup.setLogisticsNo(null);
        assertEquals(4, builder.build(List.of(pickup), name -> "SF").get("logistics_type"));
    }

    /** 快递缺单号时明确失败，避免提交空物流。 */
    @Test
    void expressRequiresTracking() {
        Order a = order("A","DELIVERED","NORMAL"); a.setLogisticsNo(null);
        assertEquals("MISSING_LOGISTICS", assertThrows(WechatOrderSyncException.class,
                () -> builder.build(List.of(a), name -> "SF")).getCode());
    }

    /** 无需配送包裹保留同城/线下交付方式，不强制填虚假快递单号。 */
    @Test
    void noDeliveryPackagesDoNotRequireTracking() {
        Order a = order("A","DELIVERED","NORMAL");
        OrderPackage pkg = new OrderPackage(); pkg.setFulfillmentMode("NO_DELIVERY");
        when(packages.orderPackageList("A")).thenReturn(List.of(pkg));
        assertEquals(2, builder.build(List.of(a), name -> "SF").get("logistics_type"));
    }

    /** 支付已关闭订单不允许借完成状态补报。 */
    @Test
    void cancelledOrderIsRejected() {
        assertEquals("INVALID_PAYMENT_STATE", assertThrows(WechatOrderSyncException.class,
                () -> builder.build(List.of(order("A","CANCELLED","NORMAL")), name -> "SF")).getCode());
    }

    /** 构造带完整物流的已支付子订单，便于单独改变待测业务条件。 */
    private Order order(String sn, String status, String type) {
        Order order = new Order(); order.setSn(sn); order.setPayStatus("PAID"); order.setOrderStatus(status); order.setOrderType(type);
        order.setDeliveryMethod("LOGISTICS"); order.setLogisticsNo("tracking-" + sn); order.setLogisticsName("顺丰"); order.setConsigneeMobile("13812345678");
        return order;
    }
}
