package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.promotion.entity.dos.BasePromotions;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.tools.PromotionTools;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import cn.lili.modules.search.service.GoodsIndexService;
import com.alibaba.fastjson2.JSON;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Date;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * 已下线活动不得通过旧促销数据或缓存继续展示、计价。
 * @author Bulbasaur
 * @since 2026-11-03
 */
class RetiredDiscountPromotionTest {
    @ParameterizedTest
    @ValueSource(strings = {"FLASH_DISCOUNT", "NTH_ITEM_DISCOUNT"})
    void ignoresRetiredActivitiesWhenBuildingPromotionMap(String type) {
        PromotionGoods goods = new PromotionGoods();
        goods.setPromotionType(type);
        goods.setPromotionId("retired-activity");
        PromotionServiceImpl service = new PromotionServiceImpl();

        assertThat(service.wrapperPromotionMapList(List.of(goods))).isEmpty();
        assertThat(PromotionTypeEnum.haveStock(type)).isFalse();
    }

    @Test
    void currentPromotionsDoNotAdvertiseRetiredActivities() {
        PromotionGoods retired = new PromotionGoods();
        retired.setPromotionType("FLASH_DISCOUNT");
        PromotionGoods active = new PromotionGoods();
        active.setPromotionType("SECKILL");
        PromotionGoodsService goodsService = mock(PromotionGoodsService.class);
        when(goodsService.listFindAll(any())).thenReturn(List.of(retired, active));
        PromotionServiceImpl service = new PromotionServiceImpl();
        ReflectionTestUtils.setField(service, "promotionGoodsService", goodsService);

        assertThat(service.getCurrentPromotion()).containsOnlyKeys("SECKILL");
    }

    @ParameterizedTest
    @ValueSource(strings = {"FLASH_DISCOUNT", "NTH_ITEM_DISCOUNT"})
    void dropsRetiredCachedActivityAndPreservesSeckill(String type) {
        BasePromotions activity = currentActivity();
        Map<String, Object> source = Map.of(type + "-old", activity, "SECKILL-active", activity);

        assertThat(PromotionTools.filterInvalidPromotionsMap(source)).containsOnlyKeys("SECKILL-active");
        assertThat(source).hasSize(2);
    }

    @ParameterizedTest
    @ValueSource(strings = {"FLASH_DISCOUNT", "NTH_ITEM_DISCOUNT"})
    void staleActivityCannotSetSkuPromotionPrice(String type) {
        GoodsIndex index = new GoodsIndex();
        index.setPromotionMapJson(JSON.toJSONString(Map.of(type + "-old", currentActivity())));
        GoodsIndexService indexService = mock(GoodsIndexService.class);
        when(indexService.findById("sku")).thenReturn(index);
        PromotionGoodsServiceImpl service = new PromotionGoodsServiceImpl();
        ReflectionTestUtils.setField(service, "goodsIndexService", indexService);
        GoodsSku sku = new GoodsSku();
        sku.setId("sku");
        sku.setPrice(100D);
        sku.setPromotionFlag(true);
        sku.setPromotionPrice(1D);

        assertThat(service.getCurrentGoodsPromotion(sku, CartTypeEnum.CART.name())).isNullOrEmpty();
        assertThat(sku.getPromotionFlag()).isFalse();
        assertThat(sku.getPromotionPrice()).isNull();
        assertThat(sku.getPrice()).isEqualTo(100D);
    }

    private BasePromotions currentActivity() {
        BasePromotions activity = new BasePromotions();
        activity.setId("activity");
        activity.setStartTime(new Date(System.currentTimeMillis() - 60_000));
        activity.setEndTime(new Date(System.currentTimeMillis() + 60_000));
        return activity;
    }
}
