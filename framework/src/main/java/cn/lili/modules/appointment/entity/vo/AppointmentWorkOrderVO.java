package cn.lili.modules.appointment.entity.vo;

import cn.lili.modules.appointment.entity.dos.AppointmentWorkOrder;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 预约服务工单视图
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "预约服务工单视图")
public class AppointmentWorkOrderVO extends AppointmentWorkOrder {

    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "服务人员姓名")
    private String staffName;
}
