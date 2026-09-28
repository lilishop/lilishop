package cn.lili.modules.appointment.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * 预约商品扩展设置，对应 {@code li_appointment_goods_ext}
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@TableName("li_appointment_goods_ext")
@Schema(description = "预约商品扩展")
public class AppointmentGoodsExt extends BaseEntity {

    @Schema(description = "商品 SPU ID")
    private String goodsId;

    @Schema(description = "店铺 ID")
    private String storeId;

    @Schema(description = "服务模式 IN_STORE,ON_SITE")
    private String serviceModes;

    @Schema(description = "是否展示剩余可约数量")
    private Boolean showRemainingQty;

    @Schema(description = "可预约日期模式")
    private String bookableDateMode;

    @Schema(description = "自定义可约起始日")
    private Date customDateStart;

    @Schema(description = "自定义可约结束日")
    private Date customDateEnd;

    @Schema(description = "自定义周几")
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

    @Schema(description = "关联系统表单 ID")
    private String systemFormId;

    @Schema(description = "关联表单快照 JSON")
    private String systemFormSnapshot;

    @Schema(description = "服务时长分钟")
    private Integer serviceDurationMinutes;

    @Schema(description = "时段划分模式 AUTO/CUSTOM")
    private String slotDivisionMode;

    @Schema(description = "自动划分开始")
    private String autoStartTime;

    @Schema(description = "自动划分结束")
    private String autoEndTime;
}
