package cn.lili.modules.appointment.entity.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.io.Serializable;

/**
 * 预约核销参数
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
public class AppointmentVerifyCodeDTO implements Serializable {

    @NotBlank(message = "核销码不能为空")
    private String code;

    @NotBlank(message = "请选择服务人员")
    private String staffId;
}
