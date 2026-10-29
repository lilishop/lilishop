package cn.lili.modules.system.entity.dos;

import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
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
 * 定时任务实体
 *
 * @author Bulbasaur
 * @since 2026-10-29
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@TableName("li_timed_task")
@Schema(description = "定时任务配置")
public class TimedTask implements Serializable {

    private static final long serialVersionUID = 1L;

    @TableId
    @Schema(description = "任务唯一标识")
    private String id;

    @Schema(description = "任务名称")
    private String taskName;

    @Schema(description = "所属分组")
    private String groupName;

    @Schema(description = "调用的 Spring Bean 名称或类名")
    private String beanName;

    @Schema(description = "调用的方法名，默认 execute")
    private String methodName;

    @Schema(description = "Cron 表达式")
    private String cron;

    @Schema(description = "状态：true 启用，false 停用")
    private Boolean status;

    @Schema(description = "任务描述")
    private String description;

    @Schema(description = "上次执行时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date lastExecTime;

    @Schema(description = "上次执行状态：1 成功，0 失败")
    private Integer lastExecStatus;

    @Schema(description = "上次执行耗时(毫秒)")
    private Long lastExecCost;

    @Schema(description = "上次执行异常信息")
    private String lastExecError;

    @Schema(description = "创建时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    @Schema(description = "更新时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date updateTime;
}
