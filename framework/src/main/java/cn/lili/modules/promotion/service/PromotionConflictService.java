package cn.lili.modules.promotion.service;

import cn.lili.common.enums.PromotionTypeEnum;

import java.util.Date;

/**
 * 促销活动互斥校验
 */
public interface PromotionConflictService {

    /**
     * 校验 SKU 是否与互斥促销类型时间重叠
     *
     * @param currentType        当前活动类型
     * @param skuId              SKU ID
     * @param startTime          开始时间
     * @param endTime            结束时间
     * @param excludePromotionId 排除的活动 ID（编辑时）
     * @param goodsName          商品名称（错误提示）
     */
    void checkSkuExclusiveConflict(PromotionTypeEnum currentType, String skuId, Date startTime, Date endTime,
                                   String excludePromotionId, String goodsName);
}
