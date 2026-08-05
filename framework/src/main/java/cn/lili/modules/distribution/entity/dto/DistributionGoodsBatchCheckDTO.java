package cn.lili.modules.distribution.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

import java.util.List;

/**
 * 批量添加分销商品参数
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionGoodsBatchCheckDTO {

    @NotEmpty(message = "分销商品列表不能为空")
    @Valid
    @Schema(description = "分销商品列表")
    private List<DistributionGoodsCheckItemDTO> items;
}
