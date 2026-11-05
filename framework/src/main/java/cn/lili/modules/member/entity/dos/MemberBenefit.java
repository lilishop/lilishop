package cn.lili.modules.member.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 客户权益
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("li_member_benefit")
@Schema(description = "客户权益")
public class MemberBenefit extends BaseEntity {

    @NotBlank
    @Size(min = 1, max = 50)
    @Schema(description = "权益名称")
    private String benefitName;

    @Size(max = 500)
    @Schema(description = "权益LOGO地址")
    private String benefitLogo;

    @Schema(description = "权益类型")
    private String benefitType;

    @Schema(description = "权益说明")
    private String benefitDesc;

    @NotNull
    @Min(1)
    @Max(9999)
    @Schema(description = "排序")
    private Integer benefitSort;

    @NotBlank
    @Schema(description = "状态 OPEN/CLOSE")
    private String benefitState;

    @Schema(description = "扩展配置(JSON)。GIFT_POINT 示例：{\"giftPoint\":100}；COUPON_PACKAGE 示例：{\"coupons\":[{\"couponId\":\"优惠券ID\",\"quantity\":2}]}，quantity 每券 1-10")
    private String benefitConfig;
}
