package cn.lili.modules.member.entity.vo;

import cn.lili.modules.member.entity.dos.MemberBenefit;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 管理端客户权益详情（券礼包类型附带优惠券明细）
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Data
@Schema(description = "客户权益详情")
public class MemberBenefitDetailVO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "权益基础信息")
    private MemberBenefit benefit;

    @Schema(description = "券礼包关联优惠券明细，仅 benefitType=COUPON_PACKAGE 时有值")
    private List<MemberBenefitCouponItemVO> couponItems;
}
