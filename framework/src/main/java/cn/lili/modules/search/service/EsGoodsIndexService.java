package cn.lili.modules.search.service;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.promotion.entity.dos.BasePromotions;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import org.elasticsearch.action.update.UpdateRequest;

import java.util.List;
import java.util.Map;

/**
 * 商品索引业务层
 *
 * @author paulG
 * @since 2020/10/14
 **/
public interface EsGoodsIndexService {

    /**
     * 全局索引初始化
     */
    void init();

    /**
     * 获取es生成索引进度
     *
     * @return
     */
    Map<String, Integer> getProgress();

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
     * 更新商品索引的的部分属性（只填写更新的字段，不需要更新的字段不要填写）
     *
     * @param id    商品索引id
     * @param goods 更新后的购买数量
     */
    void updateIndex(String id, EsGoodsIndex goods);

    /**
     * 更新商品索引的的部分属性
     *
     * @param queryFields  查询字段
     * @param updateFields 更新字段
     */
    void updateIndex(Map<String, Object> queryFields, Map<String, Object> updateFields);

    /**
     * 批量商品索引的的属性（ID 必填, 其他字段只填写更新的字段，不需要更新的字段不要填写。）
     *
     * @param goodsIndices 商品索引列表
     */
    void updateBulkIndex(List<EsGoodsIndex> goodsIndices);

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
     * 删除索引
     *
     * @param ids 商品索引id集合
     */
    void deleteIndexByIds(List<String> ids);

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
     */
    UpdateRequest updateEsGoodsIndexPromotions(String id, BasePromotions promotion, String key);

    /**
     * 更新商品索引的促销信息
     *
     * @param ids       id(skuId)
     * @param promotion 促销信息
     * @param key       促销信息的key
     */
    void updateEsGoodsIndexPromotions(List<String> ids, BasePromotions promotion, String key);

    /**
     * 根据列表更新商品索引的促销信息
     *
     * @param promotionGoodsList 促销商品列表
     * @param promotion          促销信息
     * @param key                促销信息的key
     */
    void updateEsGoodsIndexByList(List<PromotionGoods> promotionGoodsList, BasePromotions promotion, String key);

    /**
     * 更新全部商品索引的促销信息
     *
     * @param promotion 促销信息
     * @param key       促销信息的key
     */
    void updateEsGoodsIndexAllByList(BasePromotions promotion, String key);

    /**
     * 删除指定商品的促销信息
     *
     * @param skuIds        skuId列表
     * @param promotionType 促销类型
     */
    void deleteEsGoodsPromotionIndexByList(List<String> skuIds, PromotionTypeEnum promotionType);

    /**
     * 删除索引中指定的促销活动id的促销活动
     *
     * @param skuIds      商品skuId
     * @param promotionsKey 促销活动Key
     */
    void deleteEsGoodsPromotionByPromotionKey(List<String> skuIds, String promotionsKey);


    /**
     * 删除索引中指定的促销活动id的促销活动
     *
     * @param promotionsKey 促销活动Key
     */
    void deleteEsGoodsPromotionByPromotionKey(String promotionsKey);

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
     * 获取重置的商品索引
     *
     * @param goodsSku       商品sku信息
     * @param goodsParamDTOS 商品参数
     * @return 商品索引
     */
    EsGoodsIndex getResetEsGoodsIndex(GoodsSku goodsSku, List<GoodsParamsDTO> goodsParamDTOS);
}
