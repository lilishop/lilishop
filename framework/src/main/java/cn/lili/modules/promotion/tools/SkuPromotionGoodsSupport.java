package cn.lili.modules.promotion.tools;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.entity.dos.BasePromotions;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.service.PromotionConflictService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * SKU 级促销活动商品保存辅助
 */
@Component
public class SkuPromotionGoodsSupport {

    @Autowired
    private PromotionGoodsService promotionGoodsService;
    @Autowired
    private GoodsSkuService goodsSkuService;
    @Autowired
    private PromotionConflictService promotionConflictService;

    /**
     * 保存指定 SKU 促销商品，含互斥校验
     */
    public void replacePromotionGoods(BasePromotions promotion, List<PromotionGoods> promotionGoodsList,
                                      PromotionTypeEnum promotionType) {
        if (promotionGoodsList == null || promotionGoodsList.isEmpty()) {
            return;
        }
        List<PromotionGoods> promotionGoods = PromotionTools.promotionGoodsInit(
                promotionGoodsList, promotion, promotionType);
        for (PromotionGoods promotionGood : promotionGoods) {
            if (goodsSkuService.getCanPromotionGoodsSkuByIdFromCache(promotionGood.getSkuId()) == null) {
                throw new ServiceException("商品[" + promotionGood.getGoodsName() + "]不存在或处于不可售卖状态");
            }
            if (promotionGood.getPrice() != null && promotionGood.getOriginalPrice() != null
                    && promotionGood.getPrice() > promotionGood.getOriginalPrice()) {
                throw new ServiceException(ResultCode.PROMOTION_PRICE_ERROR);
            }
            promotionConflictService.checkSkuExclusiveConflict(
                    promotionType, promotionGood.getSkuId(), promotion.getStartTime(), promotion.getEndTime(),
                    promotion.getId(), promotionGood.getGoodsName());
        }
        PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
        searchParams.setPromotionId(promotion.getId());
        searchParams.setPromotionType(promotionType.name());
        promotionGoodsService.deletePromotionGoods(searchParams);
        promotionGoodsService.saveOrUpdateBatch(promotionGoods);
    }
}
