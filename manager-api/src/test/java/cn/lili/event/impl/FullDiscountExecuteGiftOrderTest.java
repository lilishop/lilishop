package cn.lili.event.impl;

import cn.hutool.core.lang.Snowflake;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.utils.SnowFlake;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.service.OrderLogService;
import cn.lili.modules.promotion.entity.dto.FullDiscountGiftItem;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 满赠 E_COUPON 赠品子单创建、多赠品件数与部分库存降级落库归一化测试。
 *
 * @author Mike
 * @date 2026-08-02
 */
class FullDiscountExecuteGiftOrderTest {

    private FullDiscountExecute execute;
    private OrderService orderService;
    private OrderItemService orderItemService;
    private GoodsSkuService goodsSkuService;
    private CardKeyService cardKeyService;
    private Cache cache;

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        execute = new FullDiscountExecute();
        orderService = mock(OrderService.class);
        orderItemService = mock(OrderItemService.class);
        goodsSkuService = mock(GoodsSkuService.class);
        cardKeyService = mock(CardKeyService.class);
        cache = mock(Cache.class);
        OrderLogService orderLogService = mock(OrderLogService.class);
        LiliMessageTemplate messageTemplate = mock(LiliMessageTemplate.class);
        RocketmqCustomProperties rocketmqCustomProperties = mock(RocketmqCustomProperties.class);

        ReflectionTestUtils.setField(execute, "orderService", orderService);
        ReflectionTestUtils.setField(execute, "orderItemService", orderItemService);
        ReflectionTestUtils.setField(execute, "goodsSkuService", goodsSkuService);
        ReflectionTestUtils.setField(execute, "cardKeyService", cardKeyService);
        ReflectionTestUtils.setField(execute, "cache", cache);
        ReflectionTestUtils.setField(execute, "orderLogService", orderLogService);
        ReflectionTestUtils.setField(execute, "liliMessageTemplate", messageTemplate);
        ReflectionTestUtils.setField(execute, "rocketmqCustomProperties", rocketmqCustomProperties);
        when(rocketmqCustomProperties.getOrderTopic()).thenReturn("order-topic");
        when(orderService.queryListByParams(any())).thenReturn(Collections.emptyList());
        ReflectionTestUtils.setField(SnowFlake.class, "snowflake", new Snowflake(1, 1));
    }

    /**
     * 验证满额赠卡密子订单按卡密规则规范化。
     */
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

        ArgumentCaptor<List<OrderItem>> itemCaptor = ArgumentCaptor.forClass(List.class);
        verify(orderItemService).saveBatch(itemCaptor.capture());
        OrderItem savedItem = itemCaptor.getValue().get(0);
        assertThat(savedItem.getSn()).startsWith("OI");
        assertThat(savedItem.getSn()).isNotEqualTo("004");
    }

    /**
     * 验证赠品订单项编号不使用商品SKU编码。
     */
    @Test
    void giftOrderItemSnNotGoodsSkuCode() {
        Order origin = originOrder();
        GoodsSku giftSku = giftSku();
        giftSku.setSn("004");

        ReflectionTestUtils.invokeMethod(execute, "giftOrderHandler",
                List.of(giftSku), origin, OrderTypeEnum.E_COUPON);

        ArgumentCaptor<List<OrderItem>> itemCaptor = ArgumentCaptor.forClass(List.class);
        verify(orderItemService).saveBatch(itemCaptor.capture());
        assertThat(itemCaptor.getValue().get(0).getSn()).startsWith("OI");
        assertThat(itemCaptor.getValue().get(0).getSn()).isNotEqualTo("004");
    }

    /**
     * 验证赠品子订单已存在时不重复创建。
     */
    @Test
    void giftOrderHandlerSkipsWhenGiftSubOrderAlreadyExists() {
        Order origin = originOrder();
        GoodsSku giftSku = giftSku();
        when(orderService.queryListByParams(any())).thenReturn(List.of(new Order()));

        ReflectionTestUtils.invokeMethod(execute, "giftOrderHandler",
                List.of(giftSku), origin, OrderTypeEnum.E_COUPON);

        verify(orderService, never()).save(any());
    }

    /**
     * 验证多赠品发放时若库存部分不足则按实际可用库存发放，库存为0的赠品自动跳过且不影响同档其他赠品。
     */
    @Test
    void orderStatusChangeFulfillsPartialStockAndSkipsZeroStockGift() {
        Order origin = originOrder();
        when(orderService.getBySn("P100")).thenReturn(origin);

        // 赠品1：申请赠送5件，实际库存仅3件 -> 应按3件发放
        GoodsSku partialPhysicalSku = new GoodsSku();
        partialPhysicalSku.setId("SKU_PARTIAL");
        partialPhysicalSku.setGoodsName("实物赠品A");
        partialPhysicalSku.setGoodsType(GoodsTypeEnum.PHYSICAL_GOODS.name());
        partialPhysicalSku.setQuantity(3);
        partialPhysicalSku.setCategoryPath("1,2,3");
        when(goodsSkuService.getGoodsSkuByIdFromCache("SKU_PARTIAL")).thenReturn(partialPhysicalSku);

        // 赠品2：申请赠送2件，实际库存0件 -> 应跳过不发
        GoodsSku zeroPhysicalSku = new GoodsSku();
        zeroPhysicalSku.setId("SKU_ZERO");
        zeroPhysicalSku.setGoodsName("实物赠品B");
        zeroPhysicalSku.setGoodsType(GoodsTypeEnum.PHYSICAL_GOODS.name());
        zeroPhysicalSku.setQuantity(0);
        zeroPhysicalSku.setCategoryPath("1,2,3");
        when(goodsSkuService.getGoodsSkuByIdFromCache(any(List.class)))
                .thenReturn(Arrays.asList(partialPhysicalSku, zeroPhysicalSku));

        FullDiscountGiftItem item1 = new FullDiscountGiftItem();
        item1.setSkuId("SKU_PARTIAL");
        item1.setNum(5);
        FullDiscountGiftItem item2 = new FullDiscountGiftItem();
        item2.setSkuId("SKU_ZERO");
        item2.setNum(2);

        CartVO cartVO = new CartVO();
        cartVO.setSn("P100");
        cartVO.setGiftItemList(Arrays.asList(item1, item2));
        when(cache.getString(any())).thenReturn(JSONUtil.toJsonStr(cartVO));

        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setOrderSn("P100");
        orderMessage.setNewStatus(OrderStatusEnum.PAID);

        execute.orderChange(orderMessage);

        ArgumentCaptor<Order> orderCaptor = ArgumentCaptor.forClass(Order.class);
        verify(orderService).save(orderCaptor.capture());
        assertThat(orderCaptor.getValue().getGoodsNum()).isEqualTo(3);

        ArgumentCaptor<List<OrderItem>> itemCaptor = ArgumentCaptor.forClass(List.class);
        verify(orderItemService).saveBatch(itemCaptor.capture());
        List<OrderItem> savedItems = itemCaptor.getValue();
        assertThat(savedItems).hasSize(1);
        assertThat(savedItems.get(0).getSkuId()).isEqualTo("SKU_PARTIAL");
        assertThat(savedItems.get(0).getNum()).isEqualTo(3);
    }

    /**
     * 验证若实物赠品在下单时已随主单落库为 OrderItem，支付事件触发时跳过生成独立的实物赠品子单，避免重复赠送。
     */
    @Test
    void orderStatusChangeSkipsPhysicalGiftWhenAlreadyAttachedToOriginOrder() {
        Order origin = originOrder();
        origin.setOrderType(OrderTypeEnum.NORMAL.name());
        when(orderService.getBySn("P100")).thenReturn(origin);

        GoodsSku physicalSku = new GoodsSku();
        physicalSku.setId("SKU_ATTACHED");
        physicalSku.setGoodsName("已随单赠品");
        physicalSku.setGoodsType(GoodsTypeEnum.PHYSICAL_GOODS.name());
        physicalSku.setQuantity(10);
        when(goodsSkuService.getGoodsSkuByIdFromCache(any(List.class)))
                .thenReturn(List.of(physicalSku));

        // 模拟主单已包含该赠品的 OrderItem（0元 + GIFT）
        OrderItem attachedItem = new OrderItem();
        attachedItem.setSkuId("SKU_ATTACHED");
        attachedItem.setFlowPrice(0.0D);
        attachedItem.setPromotionType("GIFT");
        when(orderItemService.getByOrderSn("P100")).thenReturn(List.of(attachedItem));

        FullDiscountGiftItem item = new FullDiscountGiftItem();
        item.setSkuId("SKU_ATTACHED");
        item.setNum(1);

        CartVO cartVO = new CartVO();
        cartVO.setSn("P100");
        cartVO.setGiftItemList(List.of(item));
        when(cache.getString(any())).thenReturn(JSONUtil.toJsonStr(cartVO));

        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setOrderSn("P100");
        orderMessage.setNewStatus(OrderStatusEnum.PAID);

        execute.orderChange(orderMessage);

        // 验证未调用 orderService.save() 创建独立赠品子单
        verify(orderService, never()).save(any(Order.class));
    }

    /**
     * 构造生成赠品子订单所依赖的原订单。
     *
     * @return 生成赠品子订单所依赖的原订单
     */
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

    /**
     * 构造卡密赠品SKU测试数据。
     *
     * @return 卡密赠品SKU测试数据
     */
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
