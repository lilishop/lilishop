package cn.lili.modules.appointment.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 预约时段库存
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@Schema(description = "预约时段库存")
public class AppointmentSlotStockVO implements Serializable {

    @Schema(description = "时段库存ID")
    private String id;

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "预约日期")
    private Date slotDate;

    @Schema(description = "时段开始")
    private String slotStartTime;

    @Schema(description = "时段结束")
    private String slotEndTime;

    @Schema(description = "可约数量")
    private Integer capacity;

    @Schema(description = "剩余可约数量")
    private Integer availableQty;

    @Schema(description = "是否展示剩余可约数量")
    private Boolean showRemainingQty;
}
