package cn.lili.modules.goods.support;

import cn.lili.cache.Cache;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.vos.GoodsSkuVO;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import cn.lili.modules.goods.service.GoodsSkuService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * SKU基础读取能力，统一缓存读取与详情列表组装，不依赖商品、SKU或促销业务服务。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class GoodsSkuReadSupport {
    private final GoodsSkuMapper mapper;
    private final Cache cache;
    private final GoodsSkuSpecAssembler specs;

    /**
     * 注入基础存储、缓存和无业务依赖的规格组装器。
     *
     * @param mapper SKU存储
     * @param cache SKU缓存
     * @param specs 规格组装器
     */
    public GoodsSkuReadSupport(GoodsSkuMapper mapper, Cache cache, GoodsSkuSpecAssembler specs) {
        this.mapper = mapper;
        this.cache = cache;
        this.specs = specs;
    }

    /**
     * 沿用SKU缓存键，未命中时查询存储并回填；不存在的SKU不缓存空值。
     *
     * @param id SKU主键
     * @return SKU，不存在时为null
     */
    public GoodsSku getGoodsSkuByIdFromCache(String id) {
        GoodsSku sku = (GoodsSku) cache.get(GoodsSkuService.getCacheKeys(id));
        if (sku == null) {
            sku = mapper.selectById(id);
            if (sku != null) {
                cache.put(GoodsSkuService.getCacheKeys(id), sku);
            }
        }
        return sku;
    }

    /**
     * 读取商品下未逻辑删除的SKU并复用规格转换规则，不额外过滤上下架状态。
     *
     * @param goodsId 商品主键
     * @return 商品详情SKU列表
     */
    public List<GoodsSkuVO> getGoodsListByGoodsId(String goodsId) {
        return specs.toVOList(mapper.selectList(new LambdaQueryWrapper<GoodsSku>()
                .eq(GoodsSku::getGoodsId, goodsId)));
    }
}
