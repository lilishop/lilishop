package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.CardKey;
import cn.lili.modules.goods.entity.enums.CardKeyFulfillStatusEnum;
import cn.lili.modules.goods.entity.enums.CardKeyStatusEnum;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.vo.GiftECouponOrderSummaryVO;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * enrichOrderDetail 卡密履约状态与满赠子单聚合测试。
 *
 * @author Mike
 * @date 2026-08-02
 */
class CardKeyServiceImplEnrichDetailTest {

    private CardKeyServiceImpl cardKeyService;
    private OrderMapper orderMapper;
    private OrderItemService orderItemService;

    @BeforeEach
    void setUp() {
        cardKeyService = spy(new CardKeyServiceImpl());
        orderMapper = mock(OrderMapper.class);
        orderItemService = mock(OrderItemService.class);
        ReflectionTestUtils.setField(cardKeyService, "orderMapper", orderMapper);
        ReflectionTestUtils.setField(cardKeyService, "orderItemService", orderItemService);
        ReflectionTestUtils.setField(cardKeyService, "cardKeySecretUtil", new cn.lili.modules.goods.util.CardKeySecretUtil());
    }

    @Test
    void enrichOrderDetailReturnsDeliveredCardKeys() {
        Order order = purchaseECouponOrder(OrderStatusEnum.COMPLETED.name());
        order.setSn("O200");
        OrderItem item = orderItem("O200", "item-2");
        OrderDetailVO vo = new OrderDetailVO(order, List.of(item), List.of(), null);

        CardKey cardKey = new CardKey();
        cardKey.setCardNo("NO-001");
        cardKey.setCardSecret("secret-1");
        cardKey.setStatus(CardKeyStatusEnum.ALLOCATED.name());
        cardKey.setOrderItemSn("item-2");
        doReturn(List.of(cardKey)).when(cardKeyService).list(any(Wrapper.class));

        cardKeyService.enrichOrderDetail(vo);

        OrderItem enriched = vo.getOrderItems().get(0);
        assertThat(enriched.getCardKeyFulfillStatus()).isEqualTo(CardKeyFulfillStatusEnum.DELIVERED.name());
        assertThat(enriched.getCardKeyDelivered()).isTrue();
        assertThat(enriched.getCardKeys()).hasSize(1);
        assertThat(enriched.getCardKeys().get(0).getCardNo()).isEqualTo("NO-001");
    }

    @Test
    void enrichOrderDetailMarksGiftSubOrderFailedWhenCancelled() {
        Order giftOrder = giftOrder(OrderStatusEnum.CANCELLED.name(), "满赠电子卡券发放失败");
        OrderItem item = orderItem("G001", "item-1");
        OrderDetailVO vo = new OrderDetailVO(giftOrder, List.of(item), List.of(), null);

        cardKeyService.enrichOrderDetail(vo);

        assertThat(vo.getOrderItems().get(0).getCardKeyFulfillStatus())
                .isEqualTo(CardKeyFulfillStatusEnum.FAILED.name());
        assertThat(vo.getOrderItems().get(0).getCardKeyFulfillMessage())
                .contains("主订单不受影响");
        assertThat(vo.getOrderItems().get(0).getCardKeyDelivered()).isFalse();
        assertThat(vo.getGiftECouponOrders()).isNull();
    }

    @Test
    void enrichOrderDetailAggregatesGiftSubOrderOnParentOrder() {
        Order parent = normalOrder("P001");
        Order gift = giftOrder(OrderStatusEnum.CANCELLED.name(), "满赠电子卡券发放失败");
        gift.setSn("G002");
        OrderItem parentItem = orderItem("P001", "pi-1");
        OrderItem giftItem = orderItem("G002", "gi-1");
        giftItem.setGoodsName("赠品卡密");
        giftItem.setSkuId("sku-gift");

        OrderDetailVO vo = new OrderDetailVO(parent, List.of(parentItem), List.of(), null);
        when(orderMapper.selectList(any())).thenReturn(List.of(gift));
        when(orderItemService.getByOrderSn("G002")).thenReturn(List.of(giftItem));

        cardKeyService.enrichOrderDetail(vo);

        assertThat(vo.getGiftECouponOrders()).hasSize(1);
        GiftECouponOrderSummaryVO summary = vo.getGiftECouponOrders().get(0);
        assertThat(summary.getOrderSn()).isEqualTo("G002");
        assertThat(summary.getGoodsName()).isEqualTo("赠品卡密");
        assertThat(summary.getCardKeyFulfillStatus()).isEqualTo(CardKeyFulfillStatusEnum.FAILED.name());
        assertThat(summary.getCardKeyDelivered()).isFalse();
    }

    @Test
    void enrichOrderDetailMarksPendingForPaidPurchaseOrder() {
        Order order = purchaseECouponOrder(OrderStatusEnum.PAID.name());
        OrderItem item = orderItem("O100", "item-1");
        OrderDetailVO vo = new OrderDetailVO(order, List.of(item), List.of(), null);

        cardKeyService.enrichOrderDetail(vo);

        assertThat(vo.getOrderItems().get(0).getCardKeyFulfillStatus())
                .isEqualTo(CardKeyFulfillStatusEnum.PENDING.name());
        assertThat(vo.getOrderItems().get(0).getCardKeyFulfillMessage()).contains("发放中");
    }

    private Order normalOrder(String sn) {
        Order order = new Order();
        order.setSn(sn);
        order.setOrderType(OrderTypeEnum.NORMAL.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.NORMAL.name());
        return order;
    }

    private Order purchaseECouponOrder(String status) {
        Order order = new Order();
        order.setSn("O100");
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.NORMAL.name());
        order.setOrderStatus(status);
        order.setPayStatus(PayStatusEnum.PAID.name());
        return order;
    }

    private Order giftOrder(String status, String cancelReason) {
        Order order = new Order();
        order.setSn("G001");
        order.setParentOrderSn("P001");
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.GIFT.name());
        order.setOrderStatus(status);
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setCancelReason(cancelReason);
        return order;
    }

    private OrderItem orderItem(String orderSn, String sn) {
        OrderItem item = new OrderItem();
        item.setOrderSn(orderSn);
        item.setSn(sn);
        return item;
    }
}
