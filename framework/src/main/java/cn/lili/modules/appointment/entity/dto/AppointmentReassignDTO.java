package cn.lili.modules.appointment.entity.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.io.Serializable;

/**
 * 预约改派服务人员参数
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
public class AppointmentReassignDTO implements Serializable {

    @NotBlank(message = "请选择服务人员")
    private String staffId;
}
