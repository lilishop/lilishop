package cn.lili.modules.system.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 预约订单平台设置（requirements §14 / §22）
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
public class AppointmentOrderSetting implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "服务开始前提醒买家（小时），0=关闭")
    private Integer serviceStartRemindMemberHours = 2;

    @Schema(description = "逾期未核销自动完结（小时），0=关闭")
    private Integer inStoreNoShowAutoCompleteHours = 24;
}
