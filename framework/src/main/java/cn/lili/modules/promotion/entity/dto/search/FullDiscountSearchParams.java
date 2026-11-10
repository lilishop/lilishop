package cn.lili.modules.promotion.entity.dto.search;

import cn.hutool.core.text.CharSequenceUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;

/**
 * 满优惠查询通用类
 *
 * @author paulG
 * @since 2020/8/21
 **/
@EqualsAndHashCode(callSuper = true)
@Data
public class FullDiscountSearchParams extends BasePromotionsSearchParams implements Serializable {

    private static final long serialVersionUID = -4052716630253333681L;

    @Schema(description = "活动名称")
    private String promotionName;

    @Schema(description = "活动子类型：FULL_MINUS-满减，FULL_RATE-满折，FULL_GIFT-满赠")
    private String activityType;

    @Schema(description = "是否赠优惠券")
    private Boolean couponFlag;

    @Schema(description = "优惠券id")
    private String couponId;

    @Override
    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = super.queryWrapper();
        if (CharSequenceUtil.isNotEmpty(promotionName)) {
            queryWrapper.like("promotion_name", promotionName);
        }
        if (CharSequenceUtil.isNotEmpty(activityType)) {
            queryWrapper.eq("activity_type", activityType);
        }
        if (couponFlag != null) {
            queryWrapper.eq("coupon_flag", couponFlag);
        }
        if (CharSequenceUtil.isNotEmpty(couponId)) {
            queryWrapper.eq("coupon_id", couponId);
        }
        return queryWrapper;
    }

}
