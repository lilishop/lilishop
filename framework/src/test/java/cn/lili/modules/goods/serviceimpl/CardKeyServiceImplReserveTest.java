package cn.lili.modules.goods.serviceimpl;

import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.CardKey;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.CardKeyStatusEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.mapper.CardKeyMapper;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 拼团 E_COUPON RESERVED 生命周期单元测试。
 */
class CardKeyServiceImplReserveTest {

    private CardKeyServiceImpl cardKeyService;
    private CardKeyMapper cardKeyMapper;
    private OrderMapper orderMapper;
    private OrderItemService orderItemService;
    private GoodsSkuService goodsSkuService;
    private OrderService orderService;

    @BeforeEach
    void setUp() {
        cardKeyService = spy(new CardKeyServiceImpl());
        cardKeyMapper = mock(CardKeyMapper.class);
        orderMapper = mock(OrderMapper.class);
        orderItemService = mock(OrderItemService.class);
        goodsSkuService = mock(GoodsSkuService.class);
        orderService = mock(OrderService.class);

        ReflectionTestUtils.setField(cardKeyService, "baseMapper", cardKeyMapper);
        ReflectionTestUtils.setField(cardKeyService, "orderMapper", orderMapper);
        ReflectionTestUtils.setField(cardKeyService, "orderItemService", orderItemService);
        ReflectionTestUtils.setField(cardKeyService, "goodsSkuService", goodsSkuService);
        ReflectionTestUtils.setField(cardKeyService, "orderService", orderService);

        doReturn(true).when(cardKeyService).updateById(any(CardKey.class));
        doReturn(true).when(cardKeyService).update(any());
        doReturn(0L).when(cardKeyService).count(any());
        doAnswer(invocation -> null).when(cardKeyService).syncSkuStock(any());
    }

    @Test
    void reserveByOrderMarksCardsReserved() {
        Order order = pintuanOrder("PT-001");
        OrderItem orderItem = orderItem("PT-001", "item-1", "sku-1", 1);
        CardKey cardKey = unusedCard("card-1", "sku-1");
        GoodsSku sku = eCouponSku("sku-1");

        when(orderMapper.selectForUpdateBySn("PT-001")).thenReturn(order);
        when(orderItemService.getByOrderSn("PT-001")).thenReturn(List.of(orderItem));
        when(goodsSkuService.getById("sku-1")).thenReturn(sku);
        when(cardKeyMapper.selectForAllocate("sku-1", 1)).thenReturn(List.of(cardKey));

        cardKeyService.reserveByOrder("PT-001");

        assertThat(cardKey.getStatus()).isEqualTo(CardKeyStatusEnum.RESERVED.name());
        assertThat(cardKey.getOrderSn()).isEqualTo("PT-001");
        verify(cardKeyService).syncSkuStock("sku-1");
    }

    @Test
    void confirmReservationAndCompleteMarksAllocatedAndCompletesOrder() {
        Order order = pintuanOrder("PT-002");
        CardKey reserved = reservedCard("card-2", "sku-1", "PT-002");

        when(orderMapper.selectForUpdateBySn("PT-002")).thenReturn(order);
        when(cardKeyMapper.selectReservedByOrder("PT-002")).thenReturn(List.of(reserved));

        cardKeyService.confirmReservationAndComplete("PT-002");

        assertThat(reserved.getStatus()).isEqualTo(CardKeyStatusEnum.ALLOCATED.name());
        verify(orderService).systemCompleteECoupon("PT-002");
    }

    @Test
    void releaseReservationReturnsCardsToUnused() {
        CardKey reserved = reservedCard("card-3", "sku-1", "PT-003");
        when(orderMapper.selectForUpdateBySn("PT-003")).thenReturn(pintuanOrder("PT-003"));
        when(cardKeyMapper.selectReservedByOrder("PT-003")).thenReturn(List.of(reserved));

        cardKeyService.releaseReservation("PT-003");

        verify(cardKeyService).update(any());
        verify(cardKeyService).syncSkuStock("sku-1");
        verify(orderService, never()).systemCompleteECoupon(any());
    }

    @Test
    void reserveByOrderFailsWhenPoolInsufficient() {
        Order order = pintuanOrder("PT-004");
        OrderItem orderItem = orderItem("PT-004", "item-1", "sku-1", 2);
        GoodsSku sku = eCouponSku("sku-1");

        when(orderMapper.selectForUpdateBySn("PT-004")).thenReturn(order);
        when(orderItemService.getByOrderSn("PT-004")).thenReturn(List.of(orderItem));
        when(goodsSkuService.getById("sku-1")).thenReturn(sku);
        when(cardKeyMapper.selectForAllocate("sku-1", 2)).thenReturn(List.of(unusedCard("c1", "sku-1")));

        assertThrows(ServiceException.class, () -> cardKeyService.reserveByOrder("PT-004"));
    }

    private Order pintuanOrder(String sn) {
        Order order = new Order();
        order.setSn(sn);
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.PINTUAN.name());
        order.setOrderStatus(OrderStatusEnum.PAID.name());
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setMemberId("member-1");
        return order;
    }

    private OrderItem orderItem(String orderSn, String itemSn, String skuId, int num) {
        OrderItem item = new OrderItem();
        item.setOrderSn(orderSn);
        item.setSn(itemSn);
        item.setSkuId(skuId);
        item.setNum(num);
        return item;
    }

    private GoodsSku eCouponSku(String skuId) {
        GoodsSku sku = new GoodsSku();
        sku.setId(skuId);
        sku.setGoodsType(GoodsTypeEnum.E_COUPON.name());
        return sku;
    }

    private CardKey unusedCard(String id, String skuId) {
        CardKey cardKey = new CardKey();
        cardKey.setId(id);
        cardKey.setSkuId(skuId);
        cardKey.setStatus(CardKeyStatusEnum.UNUSED.name());
        return cardKey;
    }

    private CardKey reservedCard(String id, String skuId, String orderSn) {
        CardKey cardKey = unusedCard(id, skuId);
        cardKey.setStatus(CardKeyStatusEnum.RESERVED.name());
        cardKey.setOrderSn(orderSn);
        return cardKey;
    }
}
