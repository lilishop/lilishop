package cn.lili.modules.distribution.entity.dto;

import cn.lili.common.vo.PageVO;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 分销等级变更历史查询参数
 *
 * @author Bulbasaur
 * @since 2026-08-05
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class DistributionGradeHistorySearchParams extends PageVO {

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "分销员手机号")
    private String distributionMobile;

    @Schema(description = "分销员昵称")
    private String memberName;

    @Schema(description = "变更后等级值（1-5）")
    private String gradeId;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "升级时间开始")
    private Date upgradeTimeStart;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "升级时间结束")
    private Date upgradeTimeEnd;
}
