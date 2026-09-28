package cn.lili.modules.appointment.entity.dto;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.io.Serializable;

/**
 * 预约服务人员保存参数
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
public class AppointmentServiceStaffDTO implements Serializable {

    private String id;

    @NotBlank
    private String clerkUserId;

    @NotBlank
    private String staffName;

    private String idPhotoUrl;
    private String contactPhone;
    private Integer sortOrder;
    private String status;
}
