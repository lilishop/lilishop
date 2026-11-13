package cn.lili.modules.search.serviceimpl;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.support.GoodsSkuReadSupport;
import cn.lili.modules.promotion.support.SkuPromotionQuerySupport;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import cn.lili.modules.search.service.GoodsIndexService;
import com.alibaba.fastjson2.JSON;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 商品索引视图服务，复用SKU读取、分类查询及独立促销查询组件，不依赖促销业务服务。
 */
@Service
public class GoodsIndexServiceImpl implements GoodsIndexService {

    @Autowired
    private GoodsSkuReadSupport skuReads;

    @Autowired
    private CategoryService categoryService;

    @Autowired
    private SkuPromotionQuerySupport skuPromotionQueries;

    @Override
    public GoodsIndex findById(String id) {
        GoodsSku goodsSku = skuReads.getGoodsSkuByIdFromCache(id);
        return buildGoodsIndex(goodsSku);
    }

    @Override
    public Map<String, Object> getPromotionMap(String id) {
        GoodsSku goodsSku = skuReads.getGoodsSkuByIdFromCache(id);
        return buildPromotionMap(goodsSku);
    }

    @Override
    public GoodsIndex buildGoodsIndex(GoodsSku goodsSku) {
        if (goodsSku == null || Boolean.TRUE.equals(goodsSku.getDeleteFlag())) {
            return null;
        }
        GoodsIndex goodsIndex = new GoodsIndex(goodsSku);
        fillCategoryNamePath(goodsIndex);
        Map<String, Object> promotionMap = buildPromotionMap(goodsSku);
        goodsIndex.setPromotionMapJson(JSON.toJSONString(promotionMap));
        fillPromotionPrice(goodsIndex);
        return goodsIndex;
    }

    /**
     * 按分类服务返回的路径顺序，用英文逗号拼接分类名称并回填索引视图。
     * 索引、分类路径或查询依赖缺失，以及未查到分类名称时，不覆盖原有名称路径。
     *
     * @param goodsIndex 待补充分类名称路径的商品索引视图，可为null
     */
    private void fillCategoryNamePath(GoodsIndex goodsIndex) {
        if (goodsIndex == null || categoryService == null || CharSequenceUtil.isBlank(goodsIndex.getCategoryPath())) {
            return;
        }
        List<String> names = categoryService.getCategoryNamesByPath(goodsIndex.getCategoryPath());
        if (!names.isEmpty()) {
            goodsIndex.setCategoryNamePath(names.stream().collect(Collectors.joining(",")));
        }
    }

    /**
     * 根据SKU所属店铺和主键查询促销映射，平台及店铺范围、活动类型过滤由公共查询组件统一处理。
     *
     * @param goodsSku 商品SKU，可为null
     * @return 以活动类型和活动ID为键的促销映射；SKU、查询依赖缺失或查询返回null时为空映射
     */
    private Map<String, Object> buildPromotionMap(GoodsSku goodsSku) {
        if (goodsSku == null || skuPromotionQueries == null) {
            return Collections.emptyMap();
        }
        Map<String, Object> promotionMap = skuPromotionQueries.getGoodsSkuPromotionMap(goodsSku.getStoreId(), goodsSku.getId());
        return promotionMap == null ? Collections.emptyMap() : promotionMap;
    }

    /**
     * 查询SKU当前有效的秒杀、拼团最低活动价，并以元为单位回填索引视图。
     * 不纳入优惠券、满额活动或尚未开始的活动；索引、查询依赖缺失或无有效活动价时保留原值。
     *
     * @param goodsIndex 待补充活动价格的商品索引视图，可为null
     */
    private void fillPromotionPrice(GoodsIndex goodsIndex) {
        if (goodsIndex == null || skuPromotionQueries == null) {
            return;
        }
        Double promotionPrice = skuPromotionQueries.getValidPromotionsGoodsPrice(goodsIndex.getId(),
                Arrays.asList(PromotionTypeEnum.SECKILL.name(), PromotionTypeEnum.PINTUAN.name()));
        if (promotionPrice != null) {
            goodsIndex.setPromotionPrice(promotionPrice);
        }
    }
}
