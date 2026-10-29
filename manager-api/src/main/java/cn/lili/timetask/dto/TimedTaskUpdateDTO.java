package cn.lili.timetask.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.io.Serializable;

/**
 * 定时任务更新参数
 *
 * @author Bulbasaur
 * @since 2026-10-29
 */
@Data
@Schema(description = "定时任务修改DTO")
public class TimedTaskUpdateDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @NotBlank(message = "任务ID不能为空")
    @Schema(description = "任务ID")
    private String id;

    @Schema(description = "Cron表达式")
    private String cron;

    @Schema(description = "启用状态：true开启，false暂停")
    private Boolean status;
}
