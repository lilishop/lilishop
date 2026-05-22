package cn.lili.modules.promotion.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

/**
 * 会员绑定礼品卡请求参数（卡号与兑换码）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@Schema(description = "会员绑定礼品卡参数")
public class GiftCardCashBindDTO {

    @NotBlank(message = "卡号不能为空")
    @Schema(description = "卡号", requiredMode = Schema.RequiredMode.REQUIRED)
    private String cardNo;

    @NotBlank(message = "兑换码不能为空")
    @Schema(description = "兑换码/卡密", requiredMode = Schema.RequiredMode.REQUIRED)
    private String cardSecret;
}
