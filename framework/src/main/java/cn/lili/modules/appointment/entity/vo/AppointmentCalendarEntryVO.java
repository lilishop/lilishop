package cn.lili.modules.appointment.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 服务月历当日明细
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@Schema(description = "服务月历当日明细")
public class AppointmentCalendarEntryVO implements Serializable {

    @Schema(description = "预约单号，优先用工单号")
    private String appointmentSn;

    @Schema(description = "订单编号")
    private String orderSn;

    @Schema(description = "客户姓名")
    private String customerName;

    @Schema(description = "预约时段，格式开始-结束")
    private String slotTime;

    @Schema(description = "服务人员姓名")
    private String staffName;

    @Schema(description = "月历状态 WAITING_ARRIVAL/ARRIVED/CANCELED")
    private String serviceStatus;
}
