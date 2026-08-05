package cn.lili.modules.distribution.entity.vos;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 分销等级变更历史展示对象
 *
 * @author Bulbasaur
 * @since 2026-08-05
 */
@Data
public class DistributionGradeHistoryVO {

    @Schema(description = "记录ID")
    private String id;

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "分销员昵称")
    private String memberName;

    @Schema(description = "分销员手机号")
    private String memberMobile;

    @Schema(description = "变更前等级值")
    private String previousGradeId;

    @Schema(description = "变更前等级名称")
    private String previousGradeName;

    @Schema(description = "变更后等级值")
    private String gradeId;

    @Schema(description = "变更后等级名称")
    private String gradeName;

    @Schema(description = "商品佣金比例快照")
    private BigDecimal goodsCommissionRate;

    @Schema(description = "邀请佣金比例快照")
    private BigDecimal inviteCommissionRate;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "升级时间")
    private Date upgradeTime;
}
