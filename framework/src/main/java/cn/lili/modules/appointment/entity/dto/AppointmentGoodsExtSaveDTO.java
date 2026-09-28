package cn.lili.modules.appointment.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * 预约商品扩展保存参数
 *
 * @author Bulbasaur
 * @since 2026-09-18
 */
@Data
@Schema(description = "预约商品扩展保存")
public class AppointmentGoodsExtSaveDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @NotBlank(message = "商品ID不能为空")
    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "服务模式，逗号分隔 IN_STORE,ON_SITE")
    private String serviceModes;

    @Schema(description = "是否展示剩余可约数量")
    private Boolean showRemainingQty;

    @Schema(description = "可预约日期模式")
    private String bookableDateMode;

    @Schema(description = "自定义可约起始日")
    private Date customDateStart;

    @Schema(description = "自定义可约结束日")
    private Date customDateEnd;

    @Schema(description = "自定义周几，逗号分隔")
    private String customWeekdays;

    @Schema(description = "预约日期范围天数")
    private Integer bookingDateRangeDays;

    @Schema(description = "提前预约模式")
    private String advanceBookingMode;

    @Schema(description = "须提前小时数")
    private Integer advanceBookingHours;

    @Schema(description = "取消规则模式")
    private String cancelMode;

    @Schema(description = "取消规则小时数")
    private Integer cancelHours;

    @Schema(description = "表单提交粒度")
    private String formSubmitMode;

    @Schema(description = "关联系统表单ID")
    private String systemFormId;

    @Schema(description = "关联表单快照 JSON")
    private String systemFormSnapshot;

    @Schema(description = "服务时长分钟")
    private Integer serviceDurationMinutes;

    @Schema(description = "时段划分模式 AUTO/CUSTOM")
    private String slotDivisionMode;

    @Schema(description = "自动划分开始时间")
    private String autoStartTime;

    @Schema(description = "自动划分结束时间")
    private String autoEndTime;

    @Schema(description = "时段模板列表")
    private List<AppointmentSlotTemplateDTO> slotTemplates;
}
