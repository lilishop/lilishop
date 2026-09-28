package cn.lili.modules.appointment.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 预约店铺配置，对应 {@code li_appointment_store_config}
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@TableName("li_appointment_store_config")
@Schema(description = "预约店铺配置")
public class AppointmentStoreConfig extends BaseEntity {

    private String storeId;
    private String serviceMode;
    private Boolean workOrderVisible;
    private Boolean acceptOrderEnabled;
    private String configJson;
}
