package cn.lili.modules.search.serviceimpl;

import cn.lili.modules.goods.support.GoodsSkuReadSupport;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.promotion.support.SkuPromotionQuerySupport;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Date;
import java.util.List;

import static cn.lili.modules.search.serviceimpl.GoodsSearchTestFixtures.fullDiscountPromotionMap;
import static cn.lili.modules.search.serviceimpl.GoodsSearchTestFixtures.sku;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verifyNoInteractions;

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
        GoodsSkuReadSupport goodsSkuService = mock(GoodsSkuReadSupport.class);
        SkuPromotionQuerySupport promotionService = mock(SkuPromotionQuerySupport.class);
        when(goodsSkuService.getGoodsSkuByIdFromCache("sku-1")).thenReturn(sku("sku-1", "goods-1", "测试商品"));
        when(promotionService.getGoodsSkuPromotionMap("store-1", "sku-1"))
                .thenReturn(fullDiscountPromotionMap("promo-1"));

        GoodsIndexServiceImpl service = new GoodsIndexServiceImpl();
        ReflectionTestUtils.setField(service, "skuReads", goodsSkuService);
        ReflectionTestUtils.setField(service, "skuPromotionQueries", promotionService);

        GoodsIndex result = service.findById("sku-1");

        Assertions.assertNotNull(result);
        Assertions.assertTrue(result.getPromotionMap().containsKey("FULL_DISCOUNT-promo-1"));
    }

    /** 验证移除延迟注入后，索引仍合并分类路径、促销映射及秒杀/拼团有效价格。 */
    @Test
    void buildsCategoryPathAndPromotionPriceFromIndependentQueries() {
        var category = mock(CategoryService.class);
        var promotions = mock(SkuPromotionQuerySupport.class);
        when(category.getCategoryNamesByPath("cat-1")).thenReturn(List.of("食品", "零食"));
        when(promotions.getGoodsSkuPromotionMap("store-1", "sku-1")).thenReturn(fullDiscountPromotionMap("promo-1"));
        when(promotions.getValidPromotionsGoodsPrice("sku-1", List.of("SECKILL", "PINTUAN"))).thenReturn(8.5D);
        var service = new GoodsIndexServiceImpl();
        ReflectionTestUtils.setField(service, "categoryService", category);
        ReflectionTestUtils.setField(service, "skuPromotionQueries", promotions);

        GoodsIndex result = service.buildGoodsIndex(sku("sku-1", "goods-1", "商品"));

        Assertions.assertEquals("食品,零食", result.getCategoryNamePath());
        Assertions.assertEquals(8.5D, result.getPromotionPrice());
        Assertions.assertTrue(result.getPromotionMap().containsKey("FULL_DISCOUNT-promo-1"));
    }

    /** 验证已删除的SKU不构造索引，也不加载分类或促销元数据。 */
    @Test
    void deletedSkuDoesNotLoadIndexMetadata() {
        var category = mock(CategoryService.class);
        var promotions = mock(SkuPromotionQuerySupport.class);
        var service = new GoodsIndexServiceImpl();
        ReflectionTestUtils.setField(service, "categoryService", category);
        ReflectionTestUtils.setField(service, "skuPromotionQueries", promotions);
        var deleted = sku("sku", "goods", "已删除商品");
        deleted.setDeleteFlag(true);

        Assertions.assertNull(service.buildGoodsIndex(deleted));
        verifyNoInteractions(category, promotions);
    }
}
