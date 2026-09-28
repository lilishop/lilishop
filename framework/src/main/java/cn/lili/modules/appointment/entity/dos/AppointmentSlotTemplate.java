package cn.lili.modules.appointment.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 预约时段模板，对应 {@code li_appointment_slot_template}
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@TableName("li_appointment_slot_template")
@Schema(description = "预约时段模板")
public class AppointmentSlotTemplate extends BaseEntity {

    private String goodsId;
    private String skuId;
    private String storeId;
    private String slotStartTime;
    private String slotEndTime;
    private Boolean enabled;
    private Integer capacity;
    private Integer sortOrder;
}
