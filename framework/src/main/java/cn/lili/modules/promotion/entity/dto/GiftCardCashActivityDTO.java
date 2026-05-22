package cn.lili.modules.promotion.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.Data;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 礼品卡活动创建/编辑请求参数
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@Schema(description = "礼品卡活动保存参数")
public class GiftCardCashActivityDTO {

    @NotBlank(message = "活动名称不能为空")
    @Size(max = 64, message = "活动名称过长")
    @Schema(description = "活动名称/礼品卡名称", requiredMode = Schema.RequiredMode.REQUIRED)
    private String giftCardName;

    @Schema(description = "礼品卡类型，默认 CASH", allowableValues = {"CASH", "PICKUP"})
    private String cardType;

    @NotNull(message = "面额不能为空")
    @DecimalMin(value = "0.01", message = "面额必须大于0")
    @Schema(description = "单卡面额", requiredMode = Schema.RequiredMode.REQUIRED)
    private BigDecimal faceValue;

    @Schema(description = "有效期类型，默认 LONG_TERM")
    private String validityType;

    @Schema(description = "固定到期时间（FIXED_UNTIL 时必填）")
    private Date validEndTime;

    @Schema(description = "激活后有效月数（AFTER_ACTIVATE_MONTHS 时必填）")
    private Integer validMonthsAfterActivate;

    @NotNull(message = "活动库存不能为空")
    @Min(value = 1, message = "活动库存至少为1")
    @Schema(description = "总库存（最多可制发凭证张数）", requiredMode = Schema.RequiredMode.REQUIRED)
    private Integer stockTotal;
}
