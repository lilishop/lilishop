package cn.lili.modules.search.service;

import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.BasePromotion;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;

import java.util.List;
import java.util.Map;

/**
 * 商品索引业务层
 *
 * @author paulG
 * @date 2020/10/14
 **/
public interface EsGoodsIndexService {

    /**
     * 添加商品索引
     *
     * @param goods 商品索引信息
     */
    void addIndex(EsGoodsIndex goods);

    /**
     * 更新商品索引
     *
     * @param goods 商品索引信息
     */
    void updateIndex(EsGoodsIndex goods);

    /**
     * 更新商品索引的购买数量
     *
     * @param id       商品索引id
     * @param buyCount 更新后的购买数量
     */
    void updateIndexBuyNum(String id, Integer buyCount);

    /**
     * 更新商品索引的评论相关数据
     *
     * @param id            商品索引ID
     * @param commentNum    评论数量
     * @param highPraiseNum 好评数量
     * @param grade         好评率
     */
    void updateIndexCommentNum(String id, Integer commentNum, Integer highPraiseNum, Double grade);

    /**
     * 删除索引
     *
     * @param goods 商品索引信息
     */
    void deleteIndex(EsGoodsIndex goods);

    /**
     * 删除索引
     *
     * @param id 商品索引信息
     */
    void deleteIndexById(String id);

    /**
     * 初始化商品索引
     *
     * @param goodsIndexList 商品索引列表
     */
    void initIndex(List<EsGoodsIndex> goodsIndexList);

    /**
     * 更新商品索引的促销信息
     *
     * @param id        id(skuId)
     * @param promotion 促销信息
     * @param key       促销信息的key
     * @param price     促销价格
     */
    void updateEsGoodsIndex(String id, BasePromotion promotion, String key, Double price);

    /**
     * 根据列表更新商品索引的促销信息
     *
     * @param promotionGoodsList 促销商品列表
     * @param promotion          促销信息
     * @param key                促销信息的key
     */
    void updateEsGoodsIndexByList(List<PromotionGoods> promotionGoodsList, BasePromotion promotion, String key);

    /**
     * 更新全部商品索引的促销信息
     *
     * @param promotion 促销信息
     * @param key       促销信息的key
     */
    void updateEsGoodsIndexAllByList(BasePromotion promotion, String key);

    /**
     * 删除指定商品的促销信息
     *
     * @param skuIds        skuId列表
     * @param promotionType 促销类型
     */
    void deleteEsGoodsPromotionIndexByList(List<String> skuIds, PromotionTypeEnum promotionType);

    /**
     * 删除索引中指定的促销活动id的促销活动
     * @param skuId 商品skuId
     * @param promotionId 促销活动Id
     */
    void deleteEsGoodsPromotionByPromotionId(String skuId, String promotionId);
    /**
     * 清除所以商品索引的无效促销活动
     */
    void cleanInvalidPromotion();

    /**
     * 根据id获取商品索引信息
     *
     * @param id skuId
     * @return 商品索引信息
     */
    EsGoodsIndex findById(String id);

    /**
     * 根据id获取商品索引信息的促销信息
     *
     * @param id skuId
     * @return 促销信息map
     */
    Map<String, Object> getPromotionMap(String id);

    /**
     * 根据id获取商品索引信息的指定促销活动的id
     *
     * @param id                skuId
     * @param promotionTypeEnum 促销活动类型
     * @return 当前商品参与的促销活动id集合
     */
    List<String> getPromotionIdByPromotionType(String id, PromotionTypeEnum promotionTypeEnum);

    /**
     * 重置当前商品索引
     *
     * @param goodsSku 商品sku信息
     * @param goodsParamDTOS 商品参数
     * @return 商品索引
     */
    EsGoodsIndex resetEsGoodsIndex(GoodsSku goodsSku, List<GoodsParamsDTO> goodsParamDTOS);
}
