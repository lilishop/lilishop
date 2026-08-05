package cn.lili.modules.distribution.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 分销员批量导入批次
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Schema(description = "分销员批量导入批次")
@TableName("li_distribution_import_batch")
@NoArgsConstructor
public class DistributionImportBatch extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "导入文件名")
    private String fileName;

    @Schema(description = "总行数")
    private Integer totalCount;

    @Schema(description = "成功行数")
    private Integer successCount;

    @Schema(description = "失败行数")
    private Integer failCount;

    @Schema(description = "状态：PROCESSING/DONE/FAILED")
    private String status;

    @Schema(description = "操作人ID")
    private String operatorId;

    @Schema(description = "操作人名称")
    private String operatorName;

    @Schema(description = "备注")
    private String remark;
}
