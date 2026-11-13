package cn.lili.modules.search.serviceimpl;

import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Date;

import static cn.lili.modules.search.serviceimpl.GoodsSearchTestFixtures.fullDiscountPromotionMap;
import static cn.lili.modules.search.serviceimpl.GoodsSearchTestFixtures.sku;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 商品索引构造及查询回归测试，验证上架时间赋值和促销映射重建；不连接真实数据库。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class GoodsIndexServiceImplTest {

    /**
     * 验证商品索引构造时不自动填充上架时间。
     */
    @Test
    void goodsIndexMainConstructorLeavesReleaseTimeNullUntilExplicitlySet() {
        Assertions.assertNull(new GoodsIndex(sku("sku-1", "goods-1", "测试商品")).getReleaseTime());
        Date browseTime = new Date(1234);
        Assertions.assertEquals(browseTime, new GoodsIndex(sku("sku-1", "goods-1", "测试商品"), browseTime).getReleaseTime());
    }

    /**
     * 验证无搜索索引时根据MySQL促销绑定重建促销映射。
     */
    @Test
    void noEsIndexFindByIdRebuildsPromotionMapFromMysqlPromotionBinding() {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        PromotionService promotionService = mock(PromotionService.class);
        when(goodsSkuService.getGoodsSkuByIdFromCache("sku-1")).thenReturn(sku("sku-1", "goods-1", "测试商品"));
        when(promotionService.getGoodsSkuPromotionMap("store-1", "sku-1"))
                .thenReturn(fullDiscountPromotionMap("promo-1"));

        GoodsIndexServiceImpl service = new GoodsIndexServiceImpl();
        ReflectionTestUtils.setField(service, "goodsSkuService", goodsSkuService);
        ReflectionTestUtils.setField(service, "promotionService", promotionService);

        GoodsIndex result = service.findById("sku-1");

        Assertions.assertNotNull(result);
        Assertions.assertTrue(result.getPromotionMap().containsKey("FULL_DISCOUNT-promo-1"));
    }
}
