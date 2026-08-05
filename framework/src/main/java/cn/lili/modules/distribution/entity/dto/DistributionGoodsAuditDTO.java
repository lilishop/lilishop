package cn.lili.modules.distribution.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

import java.util.List;

/**
 * 分销商品审核参数
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionGoodsAuditDTO {

    @NotEmpty(message = "分销商品ID不能为空")
    @Schema(description = "分销商品ID列表")
    private List<String> ids;

    @NotEmpty(message = "审核状态不能为空")
    @Schema(description = "审核状态：PASS已审核 REFUSE审核未通过")
    private String authStatus;

    @Schema(description = "审核备注")
    private String authRemark;
}
