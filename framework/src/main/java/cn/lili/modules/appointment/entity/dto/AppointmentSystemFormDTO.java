package cn.lili.modules.appointment.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.io.Serializable;

/**
 * 预约系统表单保存参数
 *
 * @author Bulbasaur
 * @since 2026-09-18
 */
@Data
@Schema(description = "预约系统表单")
public class AppointmentSystemFormDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "表单ID，空则新增")
    private String id;

    @NotBlank
    @Schema(description = "表单名称")
    private String formName;

    @NotBlank
    @Schema(description = "字段 JSON")
    private String formSchema;

    @Schema(description = "状态 ENABLED/DISABLED")
    private String status;
}
