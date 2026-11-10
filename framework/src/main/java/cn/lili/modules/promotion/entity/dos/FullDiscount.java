package cn.lili.modules.promotion.entity.dos;

import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.DecimalMax;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 满优惠活动实体类（包含满减活动、满折活动、满赠活动，支持最多三级阶梯）
 *
 * @author Chopper
 * @since 2020-03-19 10:44 上午
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("li_full_discount")
@Schema(description = "满优惠活动")
public class FullDiscount extends BasePromotions {

    private static final long serialVersionUID = 430433787214894166L;

    @Schema(description = "活动子类型：FULL_MINUS-满减，FULL_RATE-满折，FULL_GIFT-满赠")
    private String activityType;

    @Schema(description = "阶梯规则JSON列表（最多3级）")
    private String rulesJson;

    @DecimalMax(value = "99999999.00", message = "优惠门槛金额超出限制")
    @Schema(description = "优惠门槛金额")
    private Double fullMoney;

    @Schema(description = "活动是否减现金")
    private Boolean fullMinusFlag;

    @Schema(description = "减现金")
    private Double fullMinus;

    @Schema(description = "是否打折")
    private Boolean fullRateFlag;

    @Schema(description = "打折")
    private Double fullRate;

    @Schema(description = "是否赠送积分")
    private Boolean pointFlag;

    @Schema(description = "赠送多少积分")
    private Integer point;

    @Schema(description = "是否包邮")
    private Boolean freeFreightFlag;

    @Schema(description = "是否有赠品")
    private Boolean giftFlag;

    @Schema(description = "赠品id")
    private String giftId;

    @Schema(description = "是否赠优惠券")
    private Boolean couponFlag;

    @Schema(description = "优惠券id")
    private String couponId;

    @Schema(description = "活动标题")
    private String title;

    @Schema(description = "活动说明")
    private String description;

    public Boolean getFullMinusFlag() {
        if (fullMinusFlag == null) {
            return false;
        }
        return fullMinusFlag;
    }

    public Boolean getFullRateFlag() {
        if (fullRateFlag == null) {
            return false;
        }
        return fullRateFlag;
    }

    public Boolean getPointFlag() {
        if (pointFlag == null) {
            return false;
        }
        return pointFlag;
    }

    public Boolean getFreeFreightFlag() {
        if (freeFreightFlag == null) {
            return false;
        }
        return freeFreightFlag;
    }

    public Boolean getGiftFlag() {
        if (giftFlag == null) {
            return false;
        }
        return giftFlag;
    }

    public Boolean getCouponFlag() {
        if (couponFlag == null) {
            return false;
        }
        return couponFlag;
    }
}
