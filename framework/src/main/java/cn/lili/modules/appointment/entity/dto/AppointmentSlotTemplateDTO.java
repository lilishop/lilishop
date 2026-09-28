package cn.lili.modules.appointment.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 预约时段模板保存参数
 *
 * @author Bulbasaur
 * @since 2026-09-18
 */
@Data
@Schema(description = "预约时段模板")
public class AppointmentSlotTemplateDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "时段开始")
    private String slotStartTime;

    @Schema(description = "时段结束")
    private String slotEndTime;

    @Schema(description = "是否启用")
    private Boolean enabled;

    @Schema(description = "可约数量")
    private Integer capacity;

    @Schema(description = "排序")
    private Integer sortOrder;
}
