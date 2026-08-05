package cn.lili.modules.distribution.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

import java.util.List;

/**
 * 管理端批量设置分销等级参数
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionMemberBatchGradeDTO {

    @NotEmpty(message = "请选择分销员")
    @Schema(description = "分销员ID列表")
    private List<String> ids;

    @NotBlank(message = "分销等级不能为空")
    @Schema(description = "分销等级ID")
    private String gradeId;
}
