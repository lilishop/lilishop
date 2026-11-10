package cn.lili.modules.order.order.serviceimpl;

import cn.hutool.core.lang.Snowflake;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.vo.OrderVO;
import cn.lili.modules.promotion.entity.dto.FullDiscountGiftItem;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 满赠活动实物赠品随主订单落库单元测试
 * <p>
 * 覆盖实物赠品随单转换为 0 元 OrderItem、数量累加、售后状态置为 EXPIRED（防单独退款）、
 * 非实物（电子卡券）赠品排除及库存不足降级逻辑。
 *
 * @author Bulbasaur
 * @since 2026-11-10
 */
class OrderServiceImplPhysicalGiftTest {

    private OrderServiceImpl orderService;
    private GoodsSkuService goodsSkuService;

    /**
     * 初始化测试环境与模拟服务
     */
    @BeforeEach
    void setUp() {
        orderService = new OrderServiceImpl();
        goodsSkuService = mock(GoodsSkuService.class);
        ReflectionTestUtils.setField(orderService, "goodsSkuService", goodsSkuService);
        ReflectionTestUtils.setField(SnowFlake.class, "snowflake", new Snowflake(1, 1));
    }

    /**
     * 验证实物赠品正常随主订单落库，实付金额为0且售后状态为EXPIRED
     */
    @Test
    @DisplayName("实物赠品正常随单落库为0元OrderItem且数量累加")
    void appendPhysicalGiftOrderItemsAttachesPhysicalGiftWithZeroFlowPrice() {
        Order order = new Order();
        order.setSn("O_MAIN_001");
        order.setOrderType(OrderTypeEnum.NORMAL.name());
        order.setGoodsNum(1);

        OrderVO orderVO = new OrderVO();
        orderVO.setSn("O_MAIN_001");
        orderVO.setGoodsNum(1);
        orderVO.setOrderItems(new ArrayList<>());

        CartVO cartVO = new CartVO();
        cartVO.setSn("O_MAIN_001");
        FullDiscountVO fullDiscountVO = new FullDiscountVO();
        fullDiscountVO.setId("FD_100");
        cartVO.setFullDiscount(fullDiscountVO);

        FullDiscountGiftItem giftItem = FullDiscountGiftItem.builder()
                .skuId("SKU_GIFT_PHYSICAL")
                .goodsName("精美马克杯赠品")
                .goodsType(GoodsTypeEnum.PHYSICAL_GOODS.name())
                .price(49.0D)
                .num(2)
                .build();
        cartVO.setGiftItemList(List.of(giftItem));

        TradeDTO tradeDTO = new TradeDTO();
        tradeDTO.setSn("T_001");
        tradeDTO.setCartList(List.of(cartVO));
        tradeDTO.setOrderVO(List.of(orderVO));

        GoodsSku sku = new GoodsSku();
        sku.setId("SKU_GIFT_PHYSICAL");
        sku.setGoodsName("精美马克杯赠品");
        sku.setGoodsType(GoodsTypeEnum.PHYSICAL_GOODS.name());
        sku.setPrice(49.0D);
        sku.setQuantity(20);
        sku.setCategoryPath("1,2,3");
        sku.setThumbnail("https://img.test/mug.jpg");

        when(goodsSkuService.getGoodsSkuByIdFromCache(any(List.class))).thenReturn(List.of(sku));

        List<Order> orders = List.of(order);
        List<OrderItem> orderItems = new ArrayList<>();

        ReflectionTestUtils.invokeMethod(orderService, "appendPhysicalGiftOrderItems", orders, tradeDTO, orderItems);

        assertThat(orderItems).hasSize(1);
        OrderItem giftOrderItem = orderItems.get(0);
        assertThat(giftOrderItem.getSkuId()).isEqualTo("SKU_GIFT_PHYSICAL");
        assertThat(giftOrderItem.getGoodsName()).isEqualTo("精美马克杯赠品");
        assertThat(giftOrderItem.getNum()).isEqualTo(2);
        assertThat(giftOrderItem.getFlowPrice()).isEqualTo(0.0D);
        assertThat(giftOrderItem.getGoodsPrice()).isEqualTo(49.0D);
        assertThat(giftOrderItem.getPromotionType()).isEqualTo("GIFT");
        assertThat(giftOrderItem.getPromotionId()).isEqualTo("FD_100");
        assertThat(giftOrderItem.getAfterSaleStatus()).isEqualTo(OrderItemAfterSaleStatusEnum.EXPIRED.name());
        assertThat(giftOrderItem.getOrderSn()).isEqualTo("O_MAIN_001");
        assertThat(giftOrderItem.getTradeSn()).isEqualTo("T_001");

        // 验证主订单与 OrderVO 的总件数累加
        assertThat(order.getGoodsNum()).isEqualTo(3);
        assertThat(orderVO.getGoodsNum()).isEqualTo(3);
        assertThat(orderVO.getOrderItems()).hasSize(1);
    }

    /**
     * 验证电子卡券类型赠品不随实物主单落库（保留供支付后独立生成卡密子单）
     */
    @Test
    @DisplayName("电子卡券赠品不随实物主单落库")
    void appendPhysicalGiftOrderItemsSkipsNonPhysicalGift() {
        Order order = new Order();
        order.setSn("O_MAIN_002");
        order.setOrderType(OrderTypeEnum.NORMAL.name());
        order.setGoodsNum(1);

        CartVO cartVO = new CartVO();
        cartVO.setSn("O_MAIN_002");
        FullDiscountGiftItem eCouponGift = FullDiscountGiftItem.builder()
                .skuId("SKU_ECOUPON_GIFT")
                .goodsType(GoodsTypeEnum.E_COUPON.name())
                .num(1)
                .build();
        cartVO.setGiftItemList(List.of(eCouponGift));

        TradeDTO tradeDTO = new TradeDTO();
        tradeDTO.setSn("T_002");
        tradeDTO.setCartList(List.of(cartVO));

        GoodsSku eCouponSku = new GoodsSku();
        eCouponSku.setId("SKU_ECOUPON_GIFT");
        eCouponSku.setGoodsType(GoodsTypeEnum.E_COUPON.name());
        eCouponSku.setQuantity(50);

        when(goodsSkuService.getGoodsSkuByIdFromCache(any(List.class))).thenReturn(List.of(eCouponSku));

        List<Order> orders = List.of(order);
        List<OrderItem> orderItems = new ArrayList<>();

        ReflectionTestUtils.invokeMethod(orderService, "appendPhysicalGiftOrderItems", orders, tradeDTO, orderItems);

        assertThat(orderItems).isEmpty();
        assertThat(order.getGoodsNum()).isEqualTo(1);
    }

    /**
     * 验证实物赠品申请件数大于可用库存时，按实际可用库存降级赠送
     */
    @Test
    @DisplayName("实物赠品库存不足时按实际可用库存降级赠送")
    void appendPhysicalGiftOrderItemsDegradesToAvailableStock() {
        Order order = new Order();
        order.setSn("O_MAIN_003");
        order.setOrderType(OrderTypeEnum.NORMAL.name());
        order.setGoodsNum(2);

        CartVO cartVO = new CartVO();
        cartVO.setSn("O_MAIN_003");
        FullDiscountGiftItem giftItem = FullDiscountGiftItem.builder()
                .skuId("SKU_PARTIAL")
                .goodsType(GoodsTypeEnum.PHYSICAL_GOODS.name())
                .num(5)
                .build();
        cartVO.setGiftItemList(List.of(giftItem));

        TradeDTO tradeDTO = new TradeDTO();
        tradeDTO.setSn("T_003");
        tradeDTO.setCartList(List.of(cartVO));

        GoodsSku sku = new GoodsSku();
        sku.setId("SKU_PARTIAL");
        sku.setGoodsType(GoodsTypeEnum.PHYSICAL_GOODS.name());
        sku.setPrice(10D);
        sku.setQuantity(3); // 申请 5 件，但库存仅 3 件

        when(goodsSkuService.getGoodsSkuByIdFromCache(any(List.class))).thenReturn(List.of(sku));

        List<Order> orders = List.of(order);
        List<OrderItem> orderItems = new ArrayList<>();

        ReflectionTestUtils.invokeMethod(orderService, "appendPhysicalGiftOrderItems", orders, tradeDTO, orderItems);

        assertThat(orderItems).hasSize(1);
        assertThat(orderItems.get(0).getNum()).isEqualTo(3);
        assertThat(order.getGoodsNum()).isEqualTo(5);
    }

    /**
     * 验证实物赠品库存为0时自动跳过不生成赠品项
     */
    @Test
    @DisplayName("实物赠品库存为0时跳过不落库")
    void appendPhysicalGiftOrderItemsSkipsWhenZeroStock() {
        Order order = new Order();
        order.setSn("O_MAIN_004");
        order.setOrderType(OrderTypeEnum.NORMAL.name());
        order.setGoodsNum(1);

        CartVO cartVO = new CartVO();
        cartVO.setSn("O_MAIN_004");
        FullDiscountGiftItem giftItem = FullDiscountGiftItem.builder()
                .skuId("SKU_ZERO")
                .goodsType(GoodsTypeEnum.PHYSICAL_GOODS.name())
                .num(2)
                .build();
        cartVO.setGiftItemList(List.of(giftItem));

        TradeDTO tradeDTO = new TradeDTO();
        tradeDTO.setSn("T_004");
        tradeDTO.setCartList(List.of(cartVO));

        GoodsSku sku = new GoodsSku();
        sku.setId("SKU_ZERO");
        sku.setGoodsType(GoodsTypeEnum.PHYSICAL_GOODS.name());
        sku.setPrice(10D);
        sku.setQuantity(0); // 库存为 0

        when(goodsSkuService.getGoodsSkuByIdFromCache(any(List.class))).thenReturn(List.of(sku));

        List<Order> orders = List.of(order);
        List<OrderItem> orderItems = new ArrayList<>();

        ReflectionTestUtils.invokeMethod(orderService, "appendPhysicalGiftOrderItems", orders, tradeDTO, orderItems);

        assertThat(orderItems).isEmpty();
        assertThat(order.getGoodsNum()).isEqualTo(1);
    }
}
