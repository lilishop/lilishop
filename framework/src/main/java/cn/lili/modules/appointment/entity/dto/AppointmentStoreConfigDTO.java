package cn.lili.modules.appointment.entity.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.io.Serializable;

/**
 * 预约店铺配置参数
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
public class AppointmentStoreConfigDTO implements Serializable {

    @NotBlank
    private String serviceMode;

    private Boolean workOrderVisible;
    private Boolean acceptOrderEnabled;
    private String configJson;
}
