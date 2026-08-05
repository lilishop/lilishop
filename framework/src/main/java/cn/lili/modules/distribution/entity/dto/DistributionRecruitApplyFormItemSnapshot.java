package cn.lili.modules.distribution.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 招募申请动态填写内容快照（存于申请记录 JSON）
 *
 * @author pikachu
 */
@Data
public class DistributionRecruitApplyFormItemSnapshot {

    @Schema(description = "字段ID")
    private String fieldId;

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionRecruitFieldTypeEnum
     */
    @Schema(description = "字段类型")
    private String fieldType;

    @Schema(description = "字段名称快照")
    private String fieldName;

    @Schema(description = "填写内容")
    private String fieldValue;
}
