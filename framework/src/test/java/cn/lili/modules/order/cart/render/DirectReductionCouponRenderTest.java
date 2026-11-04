package cn.lili.modules.order.cart.render;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.order.cart.entity.dto.MemberCouponDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.render.impl.CouponRender;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.service.MemberCouponService;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Date;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 立减券（现金券 + 零门槛）在购物车渲染链路（{@link CouponRender}）中的行为测试。
 * 覆盖抵扣金额封顶逻辑与零门槛券的适用性过滤。
 */
class DirectReductionCouponRenderTest {
    /**
     * 构造一张 20 元零门槛立减券：PRICE 类型、全场通用、当前处于有效期内。
     *
     * @param platform true 为平台券，false 为店铺券
     * @return 会员优惠券
     */
    private MemberCoupon coupon(boolean platform) {
        MemberCoupon coupon = new MemberCoupon();
        coupon.setId("member-coupon");
        coupon.setCouponId("coupon");
        coupon.setCouponType("PRICE");
        coupon.setPrice(20D);
        coupon.setConsumeThreshold(0D);
        coupon.setPlatformFlag(platform);
        coupon.setStoreId("store");
        coupon.setStoreCommission(0D);
        coupon.setScopeType("ALL");
        coupon.setStartTime(new Date(System.currentTimeMillis() - 60000));
        coupon.setEndTime(new Date(System.currentTimeMillis() + 60000));
        return coupon;
    }

    /**
     * 构造只含一件选中商品的交易，商品金额与券的 SKU 适用金额均为 amount；
     * 按券的平台标记把券挂到平台券位或店铺券位。
     *
     * @param coupon 已选中的优惠券
     * @param amount 商品金额
     * @return 交易上下文
     */
    private TradeDTO trade(MemberCoupon coupon, double amount) {
        GoodsSku goods = new GoodsSku();
        goods.setId("sku");
        goods.setStoreId("store");
        goods.setPrice(amount);
        CartSkuVO sku = new CartSkuVO(goods);
        sku.setChecked(true);
        sku.setNum(1);
        sku.getPriceDetailDTO().setGoodsPrice(amount);
        TradeDTO trade = new TradeDTO();
        trade.setMemberId("member");
        trade.setSkuList(List.of(sku));
        MemberCouponDTO selected = new MemberCouponDTO(Map.of("sku", amount), coupon);
        if (coupon.getPlatformFlag()) trade.setPlatformCoupon(selected);
        else trade.getStoreCoupons().put("store", selected);
        return trade;
    }

    /**
     * mock 会员优惠券服务后执行购物车优惠券渲染。
     *
     * @param trade  交易上下文
     * @param coupon 会员持有的优惠券
     */
    private void render(TradeDTO trade, MemberCoupon coupon) {
        MemberCouponService service = mock(MemberCouponService.class);
        when(service.getMemberCoupons("member")).thenReturn(List.of(coupon));
        CouponRender render = new CouponRender();
        ReflectionTestUtils.setField(render, "memberCouponService", service);
        render.render(trade);
    }

    /**
     * 立减券抵扣金额按订单金额封顶，且不修改券面值：
     * 15 元订单只抵 15 元（券面值仍为 20 元），30 元订单抵满 20 元；
     * 平台券与店铺券行为一致，券均进入可用列表。
     */
    @Test
    void platformAndStoreCouponsCapDeductionWithoutChangingFaceValue() {
        for (boolean platform : new boolean[]{true, false}) {
            MemberCoupon coupon = coupon(platform);
            TradeDTO small = trade(coupon, 15);
            render(small, coupon);
            assertThat(small.getCanUseCoupons()).containsExactly(coupon);
            assertThat(small.getSkuList().get(0).getPriceDetailDTO().getCouponPrice()).isEqualTo(15D);
            assertThat(coupon.getPrice()).isEqualTo(20D);
            TradeDTO larger = trade(coupon, 30);
            render(larger, coupon);
            assertThat(larger.getSkuList().get(0).getPriceDetailDTO().getCouponPrice()).isEqualTo(20D);
        }
    }

    /**
     * 零门槛不等于无条件可用：店铺券店铺不匹配、指定商品券商品不匹配、
     * 券已过期时，均进入不可用列表且不会被选中使用。
     */
    @Test
    void noThresholdStillRequiresMatchingGoodsAndValidTime() {
        MemberCoupon wrongStore = coupon(false);
        wrongStore.setStoreId("other-store");
        TradeDTO trade = trade(wrongStore, 15);
        render(trade, wrongStore);
        assertThat(trade.getCanUseCoupons()).isEmpty();
        assertThat(trade.getCantUseCoupons()).hasSize(1);

        MemberCoupon wrongGoods = coupon(true);
        wrongGoods.setScopeType("PORTION_GOODS");
        wrongGoods.setScopeId("other-sku");
        trade = trade(wrongGoods, 15);
        render(trade, wrongGoods);
        assertThat(trade.getCanUseCoupons()).isEmpty();

        MemberCoupon expired = coupon(true);
        expired.setEndTime(new Date(System.currentTimeMillis() - 1000));
        trade = trade(expired, 15);
        render(trade, expired);
        assertThat(trade.getCanUseCoupons()).isEmpty();
        assertThat(trade.getPlatformCoupon()).isNull();
    }
}
