package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.promotion.service.PromotionConflictService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;

/**
 * 促销活动互斥校验实现
 */
@Service
public class PromotionConflictServiceImpl implements PromotionConflictService {

    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @Override
    public void checkSkuExclusiveConflict(PromotionTypeEnum currentType, String skuId, Date startTime, Date endTime,
                                          String excludePromotionId, String goodsName) {
        for (PromotionTypeEnum type : PromotionTypeEnum.exclusiveSkuPromotion) {
            if (type == currentType) {
                continue;
            }
            int count = promotionGoodsService.findInnerOverlapPromotionGoods(
                    type.name(), skuId, startTime, endTime, excludePromotionId);
            if (count > 0) {
                throw new ServiceException(ResultCode.PROMOTION_SKU_CONFLICT,
                        "商品[" + goodsName + "]已在重叠时间段参与" + type.description() + "活动，无法重复报名");
            }
        }
    }
}
