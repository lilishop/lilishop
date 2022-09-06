package cn.lili.modules.promotion.service;

import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 促销商品业务层
 *
 * @author Chopper
 * @since 2020/11/18 9:45 上午
 */
public interface PromotionGoodsService extends IService<PromotionGoods> {


    /**
     * 缓存商品库存key
     *
     * @param typeEnum    促销分类枚举
     * @param promotionId 促销活动Id
     * @param skuId       skuId
     * @return 缓存商品库存key
     */
    static String getPromotionGoodsStockCacheKey(PromotionTypeEnum typeEnum, String promotionId, String skuId) {
        return "{" + CachePrefix.PROMOTION_GOODS_STOCK.name() + "_" + typeEnum.name() + "}_" + promotionId + "_" + skuId;
    }

    /**
     * 获取某sku所有有效活动
     *
     * @param skuId    商品skuId
     * @param storeIds 店铺id
     * @return 促销商品集合
     */
    List<PromotionGoods> findSkuValidPromotion(String skuId, String storeIds);

    /**
     * 分页获取促销商品信息
     *
     * @param searchParams 查询参数
     * @param pageVo       分页参数
     * @return 促销商品列表
     */
    IPage<PromotionGoods> pageFindAll(PromotionGoodsSearchParams searchParams, PageVO pageVo);

    /**
     * 获取促销商品信息
     *
     * @param searchParams 查询参数
     * @return 促销商品列表
     */
    List<PromotionGoods> listFindAll(PromotionGoodsSearchParams searchParams);

    /**
     * 获取促销商品信息
     *
     * @param searchParams 查询参数
     * @return 促销商品信息
     */
    PromotionGoods getPromotionsGoods(PromotionGoodsSearchParams searchParams);


    /**
     * 获取当前有效时间特定促销类型的促销商品信息
     *
     * @param skuId          skuId
     * @param promotionTypes 特定促销类型
     * @return 促销商品信息
     */
    PromotionGoods getValidPromotionsGoods(String skuId, List<String> promotionTypes);

    /**
     * 获取当前有效时间特定促销类型的促销商品价格
     *
     * @param skuId          skuId
     * @param promotionTypes 特定促销类型
     * @return 促销商品价格
     */
    Double getValidPromotionsGoodsPrice(String skuId, List<String> promotionTypes);

    /**
     * 查询参加活动促销商品是否同时参加指定类型的活动
     *
     * @param promotionType 促销类型
     * @param skuId         skuId
     * @param startTime     开始时间
     * @param endTime       结束时间
     * @param promotionId   促销活动id(是否排除当前活动，如排除，则填写，没有的话，为null)
     * @return 共参加了几种活动
     */
    Integer findInnerOverlapPromotionGoods(String promotionType, String skuId, Date startTime, Date endTime, String promotionId);


    /**
     * 获取促销活动商品库存
     *
     * @param typeEnum    促销商品类型
     * @param promotionId 促销活动id
     * @param skuId       商品skuId
     * @return 促销活动商品库存
     */
    Integer getPromotionGoodsStock(PromotionTypeEnum typeEnum, String promotionId, String skuId);

    /**
     * 批量获取促销活动商品库存
     *
     * @param typeEnum    促销商品类型
     * @param promotionId 促销活动id
     * @param skuId       批量商品skuId
     * @return 促销活动商品库存
     */
    List<Integer> getPromotionGoodsStock(PromotionTypeEnum typeEnum, String promotionId, List<String> skuId);

    /**
     * 更新促销活动商品库存
     *
     * @param promotionGoodsList 更新促销活动商品信息
     */
    void updatePromotionGoodsStock(List<PromotionGoods> promotionGoodsList);

    /**
     * 更新促销活动商品索引
     *
     * @param promotionGoods 促销商品信息
     */
    void updatePromotionGoodsByPromotions(PromotionGoods promotionGoods);

    /**
     * 删除促销商品
     *
     * @param promotionId 促销活动id
     * @param skuIds      skuId
     */
    void deletePromotionGoods(String promotionId, List<String> skuIds);

    /**
     * 删除促销促销商品
     *
     * @param promotionIds 促销活动id
     */
    void deletePromotionGoods(List<String> promotionIds);

    /**
     * 删除商品的促销
     *
     * @param goodsIds 商品id
     */
    void deletePromotionGoodsByGoods(List<String> goodsIds);

    /**
     * 根据参数删除促销商品
     *
     * @param searchParams 查询参数
     */
    void deletePromotionGoods(PromotionGoodsSearchParams searchParams);

    /**
     * 获取当前商品促销信息
     *
     * @param dataSku 商品sku信息
     * @param cartType 购物车类型
     * @return 当前商品促销信息
     */
    Map<String, Object> getCurrentGoodsPromotion(GoodsSku dataSku, String cartType);

}