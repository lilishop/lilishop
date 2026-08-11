package cn.lili.modules.distribution.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 分销员银行卡添加DTO
 *
 * @author Bulbasaur
 * @since 2026-08-06
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "分销员银行卡添加DTO")
public class DistributionBankCardAddDTO {

    @Schema(description = "收款人姓名", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotBlank(message = "请输入收款人姓名")
    private String holderName;

    @Schema(description = "银行名称", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotBlank(message = "请输入银行名称")
    private String bankName;

    @Schema(description = "银行卡号", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotBlank(message = "请输入银行卡号")
    private String cardNo;
}
