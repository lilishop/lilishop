package cn.lili.modules.distribution.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

/**
 * 分销商品选品项
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionGoodsCheckItemDTO {

    @NotBlank(message = "规格ID不能为空")
    @Schema(description = "规格ID")
    private String skuId;

    @NotNull(message = "佣金金额不能为空")
    @Schema(description = "佣金金额")
    private Double commission;
}
