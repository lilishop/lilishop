package cn.lili.modules.appointment.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 预约服务人员，对应 {@code li_appointment_service_staff}
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@TableName("li_appointment_service_staff")
@Schema(description = "预约服务人员")
public class AppointmentServiceStaff extends BaseEntity {

    private String storeId;
    private String clerkUserId;
    private String staffName;
    private String idPhotoUrl;
    private String contactPhone;
    private Integer sortOrder;
    private String status;
}
