package cn.lili.timetask.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.Serializable;
import java.util.Date;

/**
 * 定时任务视图对象
 *
 * @author Bulbasaur
 * @since 2026-10-29
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "定时任务VO")
public class TimedTaskVO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "任务唯一标识")
    private String id;

    @Schema(description = "任务名称")
    private String name;

    @Schema(description = "任务描述")
    private String description;

    @Schema(description = "所属分组")
    private String group;

    @Schema(description = "调用的 Spring Bean 名称或类名")
    private String beanName;

    @Schema(description = "调用的方法名")
    private String methodName;

    @Schema(description = "Cron表达式")
    private String cron;

    @Schema(description = "状态：true启用，false停用")
    private Boolean status;

    @Schema(description = "上次执行时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date lastExecTime;

    @Schema(description = "上次执行状态：1成功，0失败，null未执行")
    private Integer lastExecStatus;

    @Schema(description = "上次执行异常信息")
    private String lastExecError;

    @Schema(description = "上次执行耗时(毫秒)")
    private Long lastExecCost;
}
