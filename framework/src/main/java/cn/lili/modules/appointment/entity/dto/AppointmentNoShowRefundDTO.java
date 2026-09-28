package cn.lili.modules.appointment.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import lombok.Data;

import java.io.Serializable;

/**
 * 上门爽约退款参数
 *
 * @author Bulbasaur
 * @since 2026-09-18
 */
@Data
@Schema(description = "上门爽约退款参数")
public class AppointmentNoShowRefundDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @NotBlank
    @Schema(description = "退款原因")
    private String reason;

    @NotNull
    @Positive
    @Schema(description = "退款金额")
    private Double refundAmount;

    @Schema(description = "备注")
    private String remark;
}
