package cn.lili.modules.goods.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.order.cart.entity.dto.MemberCouponDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import cn.lili.modules.store.service.StoreService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * E_COUPON 下单/结算规则校验单元测试（混单、卡池库存、促销允许等）。
 *
 * @author Mike
 * @date 2026-08-02
 */
class CardKeyServiceImplValidationTest {

    private CardKeyServiceImpl cardKeyService;

    @BeforeEach
    void setUp() {
        cardKeyService = new CardKeyServiceImpl();
    }

    @Test
    void isPurchaseTypeOrderReturnsTrueForBuyNowECoupon() {
        Order order = new Order();
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.NORMAL.name());
        order.setParentOrderSn(null);

        assertThat(cardKeyService.isPurchaseTypeOrder(order)).isTrue();
    }

    /** 积分兑换型 E_COUPON 视为购买型订单。 */
    @Test
    void isPurchaseTypeOrderReturnsTrueForPointsECoupon() {
        Order order = new Order();
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.POINTS.name());
        order.setParentOrderSn(null);

        assertThat(cardKeyService.isPurchaseTypeOrder(order)).isTrue();
    }

    /** 砍价型 E_COUPON 视为购买型订单。 */
    @Test
    void isPurchaseTypeOrderReturnsTrueForKanjiaECoupon() {
        Order order = new Order();
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.KANJIA.name());
        order.setParentOrderSn(null);

        assertThat(cardKeyService.isPurchaseTypeOrder(order)).isTrue();
    }

    /** 履约门禁要求 payStatus=PAID 且 orderStatus=PAID。 */
    @Test
    void isECouponFulfillmentEligibleRequiresPaidStatus() {
        Order order = new Order();
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setPayStatus(cn.lili.modules.order.order.entity.enums.PayStatusEnum.PAID.name());
        order.setOrderStatus(cn.lili.modules.order.order.entity.enums.OrderStatusEnum.PAID.name());

        assertThat(cardKeyService.isECouponFulfillmentEligible(order)).isTrue();

        order.setOrderStatus(cn.lili.modules.order.order.entity.enums.OrderStatusEnum.CANCELLED.name());
        assertThat(cardKeyService.isECouponFulfillmentEligible(order)).isFalse();
    }

    @Test
    void isGiftECouponOrderReturnsTrueForGiftSubOrder() {
        Order order = new Order();
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.GIFT.name());

        assertThat(cardKeyService.isGiftECouponOrder(order)).isTrue();
        assertThat(cardKeyService.isPurchaseTypeOrder(order)).isFalse();
    }

    @Test
    void isPintuanECouponOrderReturnsTrueForPintuanOrder() {
        Order order = new Order();
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.PINTUAN.name());

        assertThat(cardKeyService.isPintuanECouponOrder(order)).isTrue();
        assertThat(cardKeyService.isPurchaseTypeOrder(order)).isFalse();
    }

    @Test
    void validateTradeRejectsMixedECouponAndVirtualGoods() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildMixedTrade(GoodsTypeEnum.E_COUPON, GoodsTypeEnum.VIRTUAL_GOODS);

        ServiceException ex = assertThrows(ServiceException.class, () -> service.validateTradeForECoupon(tradeDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_MIXED_CHECKOUT);
    }

    @Test
    void validateTradeRejectsCartCheckoutForECoupon() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.CART, 1);

        ServiceException ex = assertThrows(ServiceException.class, () -> service.validateTradeForECoupon(tradeDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_E_COUPON_CART_FORBIDDEN);
    }

    @Test
    void validateTradeRejectsInsufficientPoolStock() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(0);
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.BUY_NOW, 1);

        ServiceException ex = assertThrows(ServiceException.class, () -> service.validateTradeForECoupon(tradeDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_POOL_INSUFFICIENT);
    }

    @Test
    void validateTradeAllowsBuyNowWhenStockAvailable() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(5);
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.BUY_NOW, 2);

        assertDoesNotThrow(() -> service.validateTradeForECoupon(tradeDTO));
    }

    @Test
    void validateTradeAllowsPointsCartType() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.POINTS, 1);

        assertDoesNotThrow(() -> service.validateTradeForECoupon(tradeDTO));
    }

    @Test
    void validateTradeAllowsPintuanCartType() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.PINTUAN, 1);

        assertDoesNotThrow(() -> service.validateTradeForECoupon(tradeDTO));
    }

    @Test
    void validateTradeRejectsMixedECouponAndPhysical() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildMixedTrade(GoodsTypeEnum.E_COUPON, GoodsTypeEnum.PHYSICAL_GOODS);

        ServiceException ex = assertThrows(ServiceException.class, () -> service.validateTradeForECoupon(tradeDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_MIXED_CHECKOUT);
    }

    @Test
    void validateTradeRejectsMultipleECouponSkus() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = new TradeDTO(CartTypeEnum.BUY_NOW);
        tradeDTO.getSkuList().add(buildCartSkuLine("sku-ecoupon-1", GoodsTypeEnum.E_COUPON, 1));
        tradeDTO.getSkuList().add(buildCartSkuLine("sku-ecoupon-2", GoodsTypeEnum.E_COUPON, 1));

        ServiceException ex = assertThrows(ServiceException.class, () -> service.validateTradeForECoupon(tradeDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_MULTI_E_COUPON_SKU);
    }

    @Test
    void validateTradeAllowsPromotionsAndZeroFlowPrice() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.BUY_NOW, 1);
        tradeDTO.getPriceDetailDTO().setPayPoint(100L);
        tradeDTO.getPriceDetailDTO().setDiscountPrice(10D);
        tradeDTO.getPriceDetailDTO().setCouponPrice(10D);
        tradeDTO.getPriceDetailDTO().setGoodsPrice(0D);
        tradeDTO.setPlatformCoupon(new MemberCouponDTO(Map.of("sku-1", 158D), new MemberCoupon()));
        Map<String, MemberCouponDTO> storeCoupons = new HashMap<>();
        storeCoupons.put("store-1", new MemberCouponDTO(Map.of("sku-1", 158D), new MemberCoupon()));
        tradeDTO.setStoreCoupons(storeCoupons);
        CartSkuVO cartSkuVO = tradeDTO.getSkuList().get(0);
        CartVO cartVO = new CartVO(cartSkuVO);
        cartVO.setSkuList(tradeDTO.getSkuList());
        cartVO.setFullDiscount(new FullDiscountVO(new FullDiscount()));
        tradeDTO.setCartList(new java.util.ArrayList<>(java.util.List.of(cartVO)));

        assertDoesNotThrow(() -> service.validateTradeForECoupon(tradeDTO));
    }

    @Test
    void normalizeECouponTradeClearsAddressAndPreservesCoupons() {
        CardKeyServiceImpl service = new CardKeyServiceImpl();
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.BUY_NOW, 1);
        tradeDTO.setMemberAddress(new cn.lili.modules.member.entity.dos.MemberAddress());
        tradeDTO.setPlatformCoupon(new MemberCouponDTO(Map.of("sku-1", 158D), new MemberCoupon()));
        CartVO cartVO = new CartVO(tradeDTO.getSkuList().get(0));
        cartVO.setSkuList(tradeDTO.getSkuList());
        cartVO.setDeliveryMethod(cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum.LOGISTICS.name());
        tradeDTO.setCartList(new java.util.ArrayList<>(java.util.List.of(cartVO)));

        service.normalizeECouponTrade(tradeDTO);

        assertThat(tradeDTO.getMemberAddress()).isNull();
        assertThat(tradeDTO.getPlatformCoupon()).isNotNull();
        assertThat(tradeDTO.getCartList().get(0).getDeliveryMethod())
                .isEqualTo(cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum.VIRTUAL.name());
    }

    private CardKeyServiceImpl buildServiceWithPoolStock(int stock) {
        CardKeyServiceImpl service = spy(new CardKeyServiceImpl());
        doReturn(stock).when(service).countUnusedPoolStock("sku-1");
        StoreService storeService = mock(StoreService.class);
        Store store = new Store();
        store.setStoreDisable(StoreStatusEnum.OPEN.name());
        when(storeService.getById("store-1")).thenReturn(store);
        ReflectionTestUtils.setField(service, "storeService", storeService);
        return service;
    }

    private TradeDTO buildECouponTrade(CartTypeEnum cartType, int num) {
        TradeDTO tradeDTO = new TradeDTO(cartType);
        tradeDTO.getSkuList().add(buildCartSkuLine("sku-1", GoodsTypeEnum.E_COUPON, num));
        tradeDTO.getPriceDetailDTO().setGoodsPrice(158D * num);
        return tradeDTO;
    }

    private TradeDTO buildMixedTrade(GoodsTypeEnum firstType, GoodsTypeEnum secondType) {
        TradeDTO tradeDTO = new TradeDTO(CartTypeEnum.BUY_NOW);
        tradeDTO.getSkuList().add(buildCartSkuLine("sku-ecoupon", firstType, 1));
        tradeDTO.getSkuList().add(buildCartSkuLine("sku-other", secondType, 1));
        return tradeDTO;
    }

    private CartSkuVO buildCartSkuLine(String skuId, GoodsTypeEnum goodsType, int num) {
        GoodsSku goodsSku = new GoodsSku();
        goodsSku.setId(skuId);
        goodsSku.setGoodsType(goodsType.name());
        goodsSku.setStoreId("store-1");
        CartSkuVO cartSkuVO = new CartSkuVO();
        cartSkuVO.setChecked(true);
        cartSkuVO.setNum(num);
        cartSkuVO.setGoodsSku(goodsSku);
        cartSkuVO.setStoreId("store-1");
        return cartSkuVO;
    }
}

