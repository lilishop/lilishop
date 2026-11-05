package cn.lili.modules.member.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 券礼包权益中单张优惠券展示信息
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Data
@Schema(description = "券礼包-优惠券明细")
public class MemberBenefitCouponItemVO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "优惠券ID")
    private String couponId;

    @Schema(description = "优惠券名称")
    private String couponName;

    @Schema(description = "优惠券类型 PRICE/DISCOUNT")
    private String couponType;

    @Schema(description = "减免金额（减免现金券）")
    private Double price;

    @Schema(description = "折扣（打折券，如 8.5）")
    private Double couponDiscount;

    @Schema(description = "面值展示文案")
    private String faceValueText;

    @Schema(description = "有效期展示文案")
    private String validityText;

    @Schema(description = "赠送份数（来自权益配置）")
    private Integer quantity;
}
