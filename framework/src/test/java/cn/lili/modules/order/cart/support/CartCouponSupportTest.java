package cn.lili.modules.order.cart.support;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.cart.entity.dto.MemberCouponDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import cn.lili.modules.search.service.GoodsSearchService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 购物车优惠券支持类单元测试，验证优惠券选中/取消、适用范围过滤、消费门槛校验及可用券数量统计。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class CartCouponSupportTest {

    private MemberCouponService memberCouponService;
    private GoodsSearchService goodsSearchService;
    private GoodsSkuService goodsSkuService;
    private CartCouponSupport couponSupport;

    @BeforeEach
    void setUp() {
        memberCouponService = mock(MemberCouponService.class);
        goodsSearchService = mock(GoodsSearchService.class);
        goodsSkuService = mock(GoodsSkuService.class);
        couponSupport = new CartCouponSupport(memberCouponService, goodsSearchService, goodsSkuService);
    }

    /**
     * 验证使用不存在或已过期的优惠券时抛出 COUPON_EXPIRED 异常。
     */
    @Test
    @DisplayName("优惠券不存在或已过期时抛出COUPON_EXPIRED")
    void shouldThrowWhenCouponNotFound() {
        TradeDTO tradeDTO = new TradeDTO(CartTypeEnum.CART);
        when(memberCouponService.getMemberCoupon(any())).thenReturn(null);

        assertThatThrownBy(() -> couponSupport.selectCoupon(tradeDTO, "M1", "C1", true, CartTypeEnum.CART))
                .isInstanceOf(ServiceException.class)
                .extracting(e -> ((ServiceException) e).getResultCode())
                .isEqualTo(ResultCode.COUPON_EXPIRED);
    }

    /**
     * 验证满足消费门槛的平台券可以被成功应用，且支持取消使用。
     */
    @Test
    @DisplayName("满足门槛的平台券正常应用并在取消时清除")
    void shouldApplyAndCancelPlatformCoupon() {
        TradeDTO tradeDTO = buildTradeWithSku("SKU-1", "STORE-1", 100.0, 2);
        GoodsSku sku = tradeDTO.getSkuList().get(0).getGoodsSku();
        when(goodsSkuService.getGoodsSkuByIdFromCache("SKU-1")).thenReturn(sku);

        MemberCoupon platformCoupon = new MemberCoupon();
        platformCoupon.setId("C-PLAT");
        platformCoupon.setPlatformFlag(true);
        platformCoupon.setScopeType(PromotionsScopeTypeEnum.ALL.name());
        platformCoupon.setConsumeThreshold(150.0);
        when(memberCouponService.getMemberCoupon(any())).thenReturn(platformCoupon);

        couponSupport.selectCoupon(tradeDTO, "M1", "C-PLAT", true, CartTypeEnum.CART);
        assertThat(tradeDTO.getPlatformCoupon()).isNotNull();
        assertThat(tradeDTO.getPlatformCoupon().getMemberCoupon().getId()).isEqualTo("C-PLAT");

        couponSupport.selectCoupon(tradeDTO, "M1", "C-PLAT", false, CartTypeEnum.CART);
        assertThat(tradeDTO.getPlatformCoupon()).isNull();
    }

    /**
     * 验证商品金额未达消费门槛时不会应用店铺优惠券。
     */
    @Test
    @DisplayName("未达到消费门槛时不应用店铺优惠券")
    void shouldNotApplyStoreCouponWhenBelowThreshold() {
        TradeDTO tradeDTO = buildTradeWithSku("SKU-1", "STORE-1", 50.0, 1);
        GoodsSku sku = tradeDTO.getSkuList().get(0).getGoodsSku();
        when(goodsSkuService.getGoodsSkuByIdFromCache("SKU-1")).thenReturn(sku);

        MemberCoupon storeCoupon = new MemberCoupon();
        storeCoupon.setId("C-STORE");
        storeCoupon.setPlatformFlag(false);
        storeCoupon.setStoreId("STORE-1");
        storeCoupon.setScopeType(PromotionsScopeTypeEnum.ALL.name());
        storeCoupon.setConsumeThreshold(100.0);
        when(memberCouponService.getMemberCoupon(any())).thenReturn(storeCoupon);

        couponSupport.selectCoupon(tradeDTO, "M1", "C-STORE", true, CartTypeEnum.CART);
        assertThat(tradeDTO.getStoreCoupons()).doesNotContainKey("STORE-1");
    }

    /**
     * 验证取消店铺优惠券时仅移除对应店铺的券。
     */
    @Test
    @DisplayName("取消店铺优惠券时移除指定店铺槽位")
    void shouldRemoveStoreCouponWhenUseFalse() {
        TradeDTO tradeDTO = buildTradeWithSku("SKU-1", "STORE-1", 100.0, 1);
        MemberCoupon storeCoupon = new MemberCoupon();
        storeCoupon.setId("C-STORE");
        storeCoupon.setPlatformFlag(false);
        storeCoupon.setStoreId("STORE-1");
        tradeDTO.getStoreCoupons().put("STORE-1", new MemberCouponDTO(new HashMap<>(), storeCoupon));
        when(memberCouponService.getMemberCoupon(any())).thenReturn(storeCoupon);

        couponSupport.selectCoupon(tradeDTO, "M1", "C-STORE", false, CartTypeEnum.CART);
        assertThat(tradeDTO.getStoreCoupons()).isEmpty();
    }

    /**
     * 验证可用优惠券数量统计整合单品券与全品类门槛券。
     */
    @Test
    @DisplayName("统计可用优惠券数量包含单品券与满足门槛的全品类券")
    void shouldCountAvailableCouponsCorrectly() {
        TradeDTO tradeDTO = buildTradeWithSku("SKU-1", "STORE-1", 100.0, 2);
        tradeDTO.setMemberId("M1");

        GoodsIndex goodsIndex = mock(GoodsIndex.class);
        when(goodsIndex.getPromotionMap()).thenReturn(Map.of("COUPON-C1", new Object()));
        when(goodsSearchService.getGoodsBySkuIds(eq(List.of("SKU-1")), isNull()))
                .thenReturn(List.of(goodsIndex));
        when(memberCouponService.getCurrentGoodsCanUse(eq("M1"), eq(List.of("C1")), eq(200.0)))
                .thenReturn(List.of(new MemberCoupon()));

        MemberCoupon matchedAllScope = new MemberCoupon();
        matchedAllScope.setConsumeThreshold(100.0);
        MemberCoupon unmatchedAllScope = new MemberCoupon();
        unmatchedAllScope.setConsumeThreshold(300.0);
        when(memberCouponService.getAllScopeMemberCoupon(eq("M1"), eq(List.of("STORE-1"))))
                .thenReturn(List.of(matchedAllScope, unmatchedAllScope));

        Long count = couponSupport.getCanUseCoupon(tradeDTO);
        assertThat(count).isEqualTo(2L);
    }

    private TradeDTO buildTradeWithSku(String skuId, String storeId, double price, int num) {
        TradeDTO tradeDTO = new TradeDTO(CartTypeEnum.CART);
        GoodsSku sku = new GoodsSku();
        sku.setId(skuId);
        sku.setStoreId(storeId);
        sku.setPrice(price);
        sku.setCategoryPath("1,2");
        sku.setStoreCategoryPath("10");

        CartSkuVO cartSkuVO = new CartSkuVO(sku, Collections.emptyMap());
        cartSkuVO.setStoreId(storeId);
        cartSkuVO.setPurchasePrice(price);
        cartSkuVO.setNum(num);
        cartSkuVO.setChecked(true);
        tradeDTO.getSkuList().add(cartSkuVO);
        return tradeDTO;
    }
}
