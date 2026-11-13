package cn.lili.modules.promotion.support;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.support.GoodsSkuReadSupport;
import cn.lili.modules.promotion.entity.dos.*;
import cn.lili.modules.promotion.mapper.*;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.Date;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * 独立SKU促销查询回归测试，覆盖平台与店铺范围、活动价状态、退役类型和秒杀绑定时间，不连接数据库。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class SkuPromotionQuerySupportTest {
    private final PromotionGoodsMapper bindings = mock(PromotionGoodsMapper.class);
    private final CouponMapper coupons = mock(CouponMapper.class);
    private final PintuanMapper pintuans = mock(PintuanMapper.class);
    private final FullDiscountMapper discounts = mock(FullDiscountMapper.class);
    private final SeckillMapper seckills = mock(SeckillMapper.class);
    private final SeckillApplyMapper applications = mock(SeckillApplyMapper.class);
    private final PointsGoodsMapper points = mock(PointsGoodsMapper.class);
    private final GoodsSkuReadSupport skus = mock(GoodsSkuReadSupport.class);
    private final SkuPromotionQuerySupport support = new SkuPromotionQuerySupport(bindings, coupons, pintuans,
            discounts, seckills, applications, points, skus);

    /** 验证缺少SKU时不查询促销绑定或活动实体。 */
    @Test
    void missingSkuReturnsEmptyMapWithoutPromotionReads() {
        assertThat(support.getGoodsSkuPromotionMap("store", "missing")).isEmpty();
        verifyNoInteractions(bindings, coupons, pintuans, discounts, seckills, applications, points);
    }

    /** 验证促销绑定限定店铺及平台，并保留SKU、全店和分类三种适用范围。 */
    @Test
    void mapQueryRetainsStorePlatformAndApplicabilityFilters() {
        GoodsSku sku = new GoodsSku();
        sku.setCategoryPath("first,second");
        when(skus.getGoodsSkuByIdFromCache("sku")).thenReturn(sku);
        when(bindings.selectList(any(Wrapper.class))).thenReturn(List.of());

        assertThat(support.getGoodsSkuPromotionMap("store", "sku")).isEmpty();

        ArgumentCaptor<Wrapper<PromotionGoods>> captured = ArgumentCaptor.forClass(Wrapper.class);
        verify(bindings).selectList(captured.capture());
        var query = (QueryWrapper<PromotionGoods>) captured.getValue();
        assertThat(query.getCustomSqlSegment()).contains("sku_id", "scope_type", "scope_id", "store_id");
        assertThat(query.getParamNameValuePairs()).containsValue("store").containsValue("0")
                .containsValue("sku").containsValue("ALL").containsValue("PORTION_GOODS_CATEGORY");
    }

    /** 验证最低活动价查询仍限制SKU、活动类型及已开始的有效时间区间。 */
    @Test
    void priceQueryRetainsSkuTypeAndStartedWindow() {
        when(bindings.selectPromotionsGoodsPrice(any())).thenReturn(8.5D);
        assertThat(support.getValidPromotionsGoodsPrice("sku", List.of("SECKILL", "PINTUAN"))).isEqualTo(8.5D);
        ArgumentCaptor<Wrapper<PromotionGoods>> captured = ArgumentCaptor.forClass(Wrapper.class);
        verify(bindings).selectPromotionsGoodsPrice(captured.capture());
        var query = (QueryWrapper<PromotionGoods>) captured.getValue();
        assertThat(query.getCustomSqlSegment()).contains("sku_id", "promotion_type", "start_time", "end_time");
        assertThat(query.getParamNameValuePairs()).containsValue("sku").containsValue("SECKILL").containsValue("PINTUAN");
    }

    /** 验证退役促销类型被忽略，仍支持的优惠券沿用类型加活动ID的映射键和实体内容。 */
    @Test
    void ignoresRetiredTypesAndPreservesSupportedMetadata() {
        PromotionGoods retired = new PromotionGoods();
        retired.setPromotionType("FLASH_DISCOUNT");
        PromotionGoods couponBinding = new PromotionGoods();
        couponBinding.setPromotionType("COUPON");
        couponBinding.setPromotionId("coupon");
        Coupon coupon = new Coupon();
        when(coupons.selectById("coupon")).thenReturn(coupon);
        assertThat(support.wrapperPromotionMapList(List.of(retired, couponBinding)))
                .containsOnlyKeys("COUPON-coupon").containsEntry("COUPON-coupon", coupon);
        verifyNoInteractions(seckills, applications, discounts, pintuans, points);
    }

    /** 验证秒杀存在对应SKU申请时，以商品促销绑定的起止时间构建索引元数据。 */
    @Test
    void seckillMetadataUsesBindingWindowWhenSkuApplicationExists() {
        PromotionGoods binding = new PromotionGoods();
        binding.setPromotionType("SECKILL");
        binding.setPromotionId("activity");
        binding.setSkuId("sku");
        binding.setStartTime(new Date(1000));
        binding.setEndTime(new Date(2000));
        Seckill seckill = new Seckill();
        seckill.setHours("12,8");
        when(seckills.selectById("activity")).thenReturn(seckill);
        when(applications.selectList(any(Wrapper.class))).thenReturn(List.of(new SeckillApply()));
        assertThat(support.wrapperPromotionMapList(List.of(binding))).containsEntry("SECKILL-activity", seckill);
        assertThat(seckill.getStartTime()).isEqualTo(binding.getStartTime());
        assertThat(seckill.getEndTime()).isEqualTo(binding.getEndTime());
    }
}
