package cn.lili.modules.promotion.entity.dos;

import cn.lili.modules.promotion.entity.dto.BasePromotions;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.DecimalMax;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

/**
 * 满优惠活动实体类
 *
 * @author Chopper
 * @since 2020-03-19 10:44 上午
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("li_full_discount")
@ApiModel(value = "满优惠活动")
public class FullDiscount extends BasePromotions {

    private static final long serialVersionUID = 430433787214894166L;

    @NotNull(message = "请填写优惠门槛")
    @DecimalMax(value = "99999999.00", message = "优惠券门槛金额超出限制")
    @ApiModelProperty(value = "优惠门槛金额", required = true)
    private Double fullMoney;

    @ApiModelProperty(value = "活动是否减现金")
    private Boolean isFullMinus;

    @ApiModelProperty(value = "减现金")
    private Double fullMinus;

    @ApiModelProperty(value = "是否打折")
    private Boolean isFullRate;

    @ApiModelProperty(value = "打折")
    private Double fullRate;

    @ApiModelProperty(value = "是否赠送积分")
    private Boolean isPoint;

    @ApiModelProperty(value = "赠送多少积分")
    private Integer point;

    @ApiModelProperty(value = "是否包邮")
    private Boolean isFreeFreight;

    @ApiModelProperty(value = "是否有赠品")
    private Boolean isGift;

    @ApiModelProperty(value = "赠品id")
    private String giftId;

    @ApiModelProperty(value = "是否赠优惠券")
    private Boolean isCoupon;

    @ApiModelProperty(value = "优惠券id")
    private String couponId;

    @NotEmpty(message = "请填写活动标题")
    @ApiModelProperty(value = "活动标题", required = true)
    private String title;

    @ApiModelProperty(value = "活动说明")
    private String description;


    public Boolean getIsFullMinus() {
        if (isFullMinus == null) {
            return false;
        }
        return isFullMinus;
    }

    public Boolean getIsFullRate() {
        if (isFullRate == null) {
            return false;
        }
        return isFullRate;
    }

    public Boolean getIsPoint() {
        if (isPoint == null) {
            return false;
        }
        return isPoint;
    }

    public Boolean getIsFreeFreight() {
        if (isFreeFreight == null) {
            return false;
        }
        return isFreeFreight;
    }

    public Boolean getIsGift() {
        if (isGift == null) {
            return false;
        }
        return isGift;
    }

    public Boolean getIsCoupon() {
        if (isCoupon == null) {
            return false;
        }
        return isCoupon;
    }
}