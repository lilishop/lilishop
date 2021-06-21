package cn.lili.modules.promotion.service;

import cn.lili.common.cache.CachePrefix;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.PromotionGoodsDTO;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.entity.vos.PromotionGoodsSearchParams;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.Date;
import java.util.List;

/**
 * 促销商品业务层
 *
 * @author Chopper
 * @date 2020/11/18 9:45 上午
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
     * 根据活动获取商品
     *
     * @param promotionId 促销活动id
     * @param skuId       商品id
     * @return 促销商品信息
     */
    PromotionGoods findByPromotion(String promotionId, String skuId);

    /**
     * 删除指定促销类型的促销商品
     *
     * @param promotionGoodsList 促销商品列表
     * @param promotionType      促销类型
     */
    void removePromotionGoods(List<PromotionGoods> promotionGoodsList, PromotionTypeEnum promotionType);

    /**
     * 更新促销活动
     *
     * @param cartSkuVO 购物车中的产品
     */
    void updatePromotion(CartSkuVO cartSkuVO);

    /**
     * 获取购物车商品的促销活动
     *
     * @param cartSkuVO 购物车中的产品
     */
    void getCartSkuPromotion(CartSkuVO cartSkuVO);

    /**
     * 获取某sku当日所有活动
     *
     * @param skuId 商品skuId
     * @return 促销商品集合
     */
    List<PromotionGoods> findNowSkuPromotion(String skuId);

    /**
     * 分页获取促销商品信息
     *
     * @param goodsId 商品skuId
     * @return 某商品的促销信息
     */
    List<PromotionGoods> getPromotionGoods(String goodsId);

    /**
     * 分页获取促销商品信息
     *
     * @param searchParams 查询参数
     * @param pageVo       分页参数
     * @return 促销商品列表
     */
    IPage<PromotionGoodsDTO> getPromotionGoods(PromotionGoodsSearchParams searchParams, PageVO pageVo);

    /**
     * 分页获取当前进行中的促销活动的促销商品信息
     *
     * @param promotionType 促销活动类型
     * @param pageVo        分页参数
     * @return 促销商品列表
     */
    IPage<PromotionGoodsDTO> getCurrentPromotionGoods(String promotionType, PageVO pageVo);

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
     * 根据条件获取促销活动商品详情
     *
     * @param typeEnum    促销类型
     * @param promotionId 促销活动id
     * @param skuId       商品skuId
     * @return 促销活动商品详情
     */
    PromotionGoods getPromotionGoods(PromotionTypeEnum typeEnum, String promotionId, String skuId);

    /**
     * 更新促销活动商品库存
     *
     * @param typeEnum    促销商品类型
     * @param promotionId 促销活动id
     * @param skuId       商品skuId
     * @param quantity    更新后的库存数量
     */
    void updatePromotionGoodsStock(PromotionTypeEnum typeEnum, String promotionId, String skuId, Integer quantity);

}