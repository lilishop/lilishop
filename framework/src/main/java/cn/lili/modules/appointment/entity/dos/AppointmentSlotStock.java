package cn.lili.modules.appointment.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.baomidou.mybatisplus.annotation.Version;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * 预约时段日历库存，对应 {@code li_appointment_slot_stock}
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@TableName("li_appointment_slot_stock")
@Schema(description = "预约时段库存")
public class AppointmentSlotStock extends BaseEntity {

    private String goodsId;
    private String skuId;
    private String storeId;
    private Date slotDate;
    private String slotStartTime;
    private String slotEndTime;
    private Integer capacity;
    private Integer availableQty;

    @Version
    private Integer version;
}
