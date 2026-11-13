package cn.lili.modules.goods.support;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.mapper.GoodsMapper;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import cn.lili.modules.system.aspect.annotation.SystemLogPoint;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 商品库存汇总能力，统一汇总SKU库存及清理商品详情缓存，避免库存组件回调商品服务。
 * 写入沿用调用方事务，不重新开启独立事务。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class GoodsStockAggregateSupport {
    private final GoodsMapper goods;
    private final GoodsSkuMapper skus;
    private final Cache cache;

    /**
     * 注入商品和SKU存储及商品缓存。
     *
     * @param goods 商品存储
     * @param skus SKU存储
     * @param cache 商品缓存
     */
    public GoodsStockAggregateSupport(GoodsMapper goods, GoodsSkuMapper skus, Cache cache) {
        this.goods = goods;
        this.skus = skus;
        this.cache = cache;
    }

    /**
     * 按现有口径汇总全部未逻辑删除SKU的库存，空库存字段按零处理。
     *
     * @param goodsId 商品主键
     * @return 库存数量，单位件
     */
    public Integer getGoodsStock(String goodsId) {
        List<GoodsSku> rows = skus.selectList(new LambdaQueryWrapper<GoodsSku>()
                .eq(GoodsSku::getGoodsId, goodsId));
        int total = 0;
        if (rows != null) {
            for (GoodsSku sku : rows) {
                if (sku.getQuantity() != null) {
                    total += sku.getQuantity();
                }
            }
        }
        return total;
    }

    /**
     * 汇总SKU库存后更新商品库存，沿用先删除详情缓存再写库的规则及业务日志。
     *
     * @param goodsId 商品主键
     */
    @SystemLogPoint(description = "同步商品库存", customerLog = "'同步商品商品ID的库存:['+#goodsId+']'")
    public void updateStock(String goodsId) {
        Integer stock = getGoodsStock(goodsId);
        cache.remove(CachePrefix.GOODS.getPrefix() + goodsId);
        goods.update(null, new LambdaUpdateWrapper<Goods>().eq(Goods::getId, goodsId)
                .set(Goods::getQuantity, stock));
    }
}
