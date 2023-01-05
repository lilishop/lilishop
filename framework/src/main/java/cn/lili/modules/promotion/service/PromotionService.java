package cn.lili.modules.promotion.service;

import cn.lili.modules.promotion.entity.dos.PromotionGoods;

import java.util.List;
import java.util.Map;

/**
 * 促销业务层
 *
 * @author Chopper
 * @since 2020/11/18 9:45 上午
 */
public interface PromotionService {

    /**
     * 获取当前进行的所有促销活动信息
     *
     * @return 当前促销活动集合
     */
    Map<String, List<PromotionGoods>> getCurrentPromotion();

    /**
     * 根据商品索引获取当前商品索引的所有促销活动信息
     *
     * @param storeId    店铺id
     * @param goodsSkuId 商品skuId
     * @return 当前促销活动集合
     */
    Map<String, Object> getGoodsSkuPromotionMap(String storeId, String goodsSkuId);

    /**
     * 删除商品，则删除相关促销信息
     *
     * @param goodsIdsJsonStr
     */
    void removeByGoodsIds(String goodsIdsJsonStr);

    /**
     * 根据促销商品信息包装促销信息
     *
     * @param promotionGoodsList 促销商品信息
     * @return 促销信息
     */
    Map<String, Object> wrapperPromotionMapList(List<PromotionGoods> promotionGoodsList);
}