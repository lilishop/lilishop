package cn.lili.modules.distribution.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 分销等级变更历史
 *
 * @author pikachu
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Schema(description = "分销等级变更历史")
@TableName("li_distribution_grade_history")
@NoArgsConstructor
public class DistributionGradeHistory extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "变更后等级ID")
    private String gradeId;

    @Schema(description = "变更前等级ID")
    private String previousGradeId;

    @Schema(description = "等级名称快照")
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
