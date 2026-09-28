package cn.lili.modules.appointment.entity.vo;

import cn.lili.modules.appointment.entity.dos.AppointmentGoodsExt;
import cn.lili.modules.appointment.entity.dos.AppointmentSlotTemplate;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.List;

/**
 * 预约商品扩展视图
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "预约商品扩展视图")
public class AppointmentGoodsExtVO extends AppointmentGoodsExt {

    @Schema(description = "时段模板列表")
    private List<AppointmentSlotTemplate> slotTemplates;

    @Schema(description = "上门服务已开启电子围栏，结算须地图选点")
    private Boolean serviceAreaRequired;
}
