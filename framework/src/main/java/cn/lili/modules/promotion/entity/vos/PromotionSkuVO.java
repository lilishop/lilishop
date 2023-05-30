package cn.lili.modules.promotion.entity.vos;

import cn.lili.common.enums.PromotionTypeEnum;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 促销skuVO
 *
 * @author Chopper
 * @version v1.0
 * 2021-08-28 03:24
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class PromotionSkuVO implements Serializable {

    private static final long serialVersionUID = -8587010496940375179L;

    /**
     * 促销类型
     * @see PromotionTypeEnum
     */
    private String promotionType;

    /**
     * 促销活动
     */
    private String activityId;

}
