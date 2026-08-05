package cn.lili.modules.system.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 分销招募申请动态字段配置
 *
 * @author pikachu
 */
@Data
public class DistributionRecruitFieldSetting implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "字段ID（稳定标识，用于申请提交关联）")
    private String id;

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionRecruitFieldTypeEnum
     */
    @Schema(description = "字段类型")
    private String fieldType;

    @Schema(description = "字段名称")
    private String fieldName;

    @Schema(description = "是否必填")
    private Boolean required;

    @Schema(description = "排序值")
    private Integer sortOrder;

    @Schema(description = "提示文案")
    private String placeholder;
}
