package cn.lili.modules.appointment.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 服务月历单日汇总
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@Schema(description = "服务月历单日汇总")
public class AppointmentCalendarDaySummaryVO implements Serializable {

    @Schema(description = "预约日期，格式 yyyy-MM-dd")
    private String date;

    @Schema(description = "当日预约人数")
    private Integer headcount;
}
