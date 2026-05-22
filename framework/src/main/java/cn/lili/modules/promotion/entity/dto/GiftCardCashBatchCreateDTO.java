package cn.lili.modules.promotion.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.Data;

/**
 * 在活动下批量制卡请求参数（面额与规则以活动为准）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@Schema(description = "在活动下批量制卡参数（面额与规则以活动为准）")
public class GiftCardCashBatchCreateDTO {

    @NotNull(message = "制卡数量不能为空")
    @Min(value = 1, message = "制卡数量至少为1")
    @Max(value = 10000, message = "单次制卡不超过10000张")
    @Schema(description = "制卡数量", requiredMode = Schema.RequiredMode.REQUIRED)
    private Integer quantity;

    @Size(max = 64, message = "本批备注过长")
    @Schema(description = "本批备注（可选）")
    private String batchRemark;
}