package cn.lili.modules.appointment.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import lombok.Data;

import java.io.Serializable;

/**
 * 平台预约退款参数
 *
 * @author Bulbasaur
 * @since 2026-09-18
 */
@Data
@Schema(description = "平台预约退款参数")
public class AppointmentPlatformRefundDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @NotBlank
    @Schema(description = "退款类型 UNSERVED 未服务 / SERVED 已服务")
    private String refundType;

    @NotNull
    @Positive
    @Schema(description = "退款金额")
    private Double refundAmount;

    @NotBlank
    @Schema(description = "退款原因")
    private String reason;

    @Schema(description = "备注")
    private String remark;
}
