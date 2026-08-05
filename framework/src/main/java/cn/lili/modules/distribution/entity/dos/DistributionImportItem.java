package cn.lili.modules.distribution.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 分销员批量导入明细
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Schema(description = "分销员批量导入明细")
@TableName("li_distribution_import_item")
@NoArgsConstructor
public class DistributionImportItem extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "批次ID")
    private String batchId;

    @Schema(description = "Excel行号")
    private Integer rowNum;

    @Schema(description = "手机号")
    private String mobile;

    @Schema(description = "分销员openid")
    private String openId;

    @Schema(description = "昵称")
    private String nickname;

    @Schema(description = "邀请人手机号")
    private String inviterMobile;

    @Schema(description = "邀请人openid")
    private String inviterOpenId;

    @Schema(description = "等级序号")
    private Integer gradeSort;

    @Schema(description = "成功后的分销员ID")
    private String distributionId;

    @Schema(description = "状态：SUCCESS/FAIL")
    private String status;

    @Schema(description = "失败原因")
    private String failReason;
}
