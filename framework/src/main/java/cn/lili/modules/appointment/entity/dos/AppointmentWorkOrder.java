package cn.lili.modules.appointment.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 预约服务工单，对应 {@code li_appointment_work_order}
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@TableName("li_appointment_work_order")
@Schema(description = "预约服务工单")
public class AppointmentWorkOrder extends BaseEntity {

    @Schema(description = "工单号，与订单 1:1")
    private String workOrderSn;

    @Schema(description = "订单编号")
    private String orderSn;

    @Schema(description = "买家会员 ID")
    private String memberId;

    @Schema(description = "店铺 ID")
    private String storeId;

    /**
     * @see cn.lili.modules.appointment.entity.enums.AppointmentServiceModeEnum
     */
    @Schema(description = "服务模式 IN_STORE/ON_SITE")
    private String serviceMode;

    /**
     * @see cn.lili.modules.appointment.entity.enums.AppointmentWorkOrderStatusEnum
     */
    @Schema(description = "工单状态")
    private String serviceStatus;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "预约时间快照")
    private Date appointmentTime;

    @Schema(description = "预约人姓名")
    private String bookerName;

    @Schema(description = "预约人电话")
    private String bookerPhone;

    @Schema(description = "上门服务地址快照")
    private String appointmentAddress;

    @Schema(description = "服务人员 ID")
    private String staffId;

    @Schema(description = "备注")
    private String remark;
}
