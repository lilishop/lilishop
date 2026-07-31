/**
 * 作者：Mike
 * 日期：2026-07-31
 */
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
 * E_COUPON 下单/结算规则校验单元测试（混单、促销、poolStock、0 元等）。
 *
 * @author Mike
 * @date 2026-07-31
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

    @Test
    void isPurchaseTypeOrderReturnsFalseForGiftSubOrder() {
        Order order = new Order();
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.GIFT.name());
        order.setParentOrderSn("PARENT001");

        assertThat(cardKeyService.isPurchaseTypeOrder(order)).isFalse();
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
    void validateTradeRejectsMixedECouponAndPhysical() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildMixedTrade(GoodsTypeEnum.E_COUPON, GoodsTypeEnum.PHYSICAL_GOODS);

        ServiceException ex = assertThrows(ServiceException.class, () -> service.validateTradeForECoupon(tradeDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_MIXED_CHECKOUT);
    }

    @Test
    void validateTradeRejectsMixedECouponAndVirtualGoods() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildMixedTrade(GoodsTypeEnum.E_COUPON, GoodsTypeEnum.VIRTUAL_GOODS);

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
    void validateTradeSkipsWhenNoECouponInTrade() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = new TradeDTO(CartTypeEnum.CART);
        tradeDTO.getSkuList().add(buildCartSkuLine("sku-physical", GoodsTypeEnum.PHYSICAL_GOODS, 1));

        assertDoesNotThrow(() -> service.validateTradeForECoupon(tradeDTO));
    }

    @Test
    void validateTradeRejectsPayPointDeduction() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.BUY_NOW, 1);
        tradeDTO.getPriceDetailDTO().setPayPoint(100L);

        ServiceException ex = assertThrows(ServiceException.class, () -> service.validateTradeForECoupon(tradeDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
    }

    @Test
    void validateTradeRejectsDiscountPrice() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.BUY_NOW, 1);
        tradeDTO.getPriceDetailDTO().setDiscountPrice(10D);

        ServiceException ex = assertThrows(ServiceException.class, () -> service.validateTradeForECoupon(tradeDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
    }

    @Test
    void validateTradeRejectsPlatformCoupon() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.BUY_NOW, 1);
        tradeDTO.setPlatformCoupon(new MemberCouponDTO(Map.of("sku-1", 158D), new MemberCoupon()));

        ServiceException ex = assertThrows(ServiceException.class, () -> service.validateTradeForECoupon(tradeDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
    }

    @Test
    void validateTradeRejectsStoreCoupon() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.BUY_NOW, 1);
        Map<String, MemberCouponDTO> storeCoupons = new HashMap<>();
        storeCoupons.put("store-1", new MemberCouponDTO(Map.of("sku-1", 158D), new MemberCoupon()));
        tradeDTO.setStoreCoupons(storeCoupons);

        ServiceException ex = assertThrows(ServiceException.class, () -> service.validateTradeForECoupon(tradeDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
    }

    @Test
    void validateTradeRejectsCouponPrice() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.BUY_NOW, 1);
        tradeDTO.getPriceDetailDTO().setCouponPrice(10D);

        ServiceException ex = assertThrows(ServiceException.class, () -> service.validateTradeForECoupon(tradeDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
    }

    @Test
    void validateTradeRejectsCartFullDiscount() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.BUY_NOW, 1);
        CartSkuVO cartSkuVO = tradeDTO.getSkuList().get(0);
        CartVO cartVO = new CartVO(cartSkuVO);
        cartVO.setSkuList(tradeDTO.getSkuList());
        cartVO.setFullDiscount(new FullDiscountVO(new FullDiscount()));
        tradeDTO.setCartList(new java.util.ArrayList<>(java.util.List.of(cartVO)));

        ServiceException ex = assertThrows(ServiceException.class, () -> service.validateTradeForECoupon(tradeDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
    }

    @Test
    void validateTradeRejectsPointsCartType() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.POINTS, 1);

        ServiceException ex = assertThrows(ServiceException.class, () -> service.validateTradeForECoupon(tradeDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_E_COUPON_CART_FORBIDDEN);
    }

    @Test
    void validateTradeRejectsZeroFlowPrice() {
        CardKeyServiceImpl service = buildServiceWithPoolStock(10);
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.BUY_NOW, 1);
        tradeDTO.getPriceDetailDTO().setGoodsPrice(0D);

        ServiceException ex = assertThrows(ServiceException.class, () -> service.validateTradeForECoupon(tradeDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_ZERO_PRICE);
    }

    @Test
    void normalizeECouponTradeClearsAddressAndSetsVirtualDelivery() {
        CardKeyServiceImpl service = new CardKeyServiceImpl();
        TradeDTO tradeDTO = buildECouponTrade(CartTypeEnum.BUY_NOW, 1);
        tradeDTO.setMemberAddress(new cn.lili.modules.member.entity.dos.MemberAddress());
        cn.lili.modules.order.cart.entity.vo.CartVO cartVO = new cn.lili.modules.order.cart.entity.vo.CartVO(tradeDTO.getSkuList().get(0));
        cartVO.setSkuList(tradeDTO.getSkuList());
        cartVO.setDeliveryMethod(cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum.LOGISTICS.name());
        tradeDTO.setCartList(new java.util.ArrayList<>(java.util.List.of(cartVO)));

        service.normalizeECouponTrade(tradeDTO);

        assertThat(tradeDTO.getMemberAddress()).isNull();
        assertThat(tradeDTO.getCartList().get(0).getDeliveryMethod())
                .isEqualTo(cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum.VIRTUAL.name());
        assertThat(service.isECouponOnlyTrade(tradeDTO)).isTrue();
    }

    @Test
    void isECouponOnlyTradeReturnsFalseForMixedSkuList() {
        CardKeyServiceImpl service = new CardKeyServiceImpl();
        TradeDTO tradeDTO = buildMixedTrade(GoodsTypeEnum.E_COUPON, GoodsTypeEnum.PHYSICAL_GOODS);
        assertThat(service.isECouponOnlyTrade(tradeDTO)).isFalse();
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
