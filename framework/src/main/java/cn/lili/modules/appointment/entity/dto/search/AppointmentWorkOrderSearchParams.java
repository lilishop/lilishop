package cn.lili.modules.appointment.entity.dto.search;

import cn.lili.common.vo.PageVO;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 工单搜索参数
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "工单搜索参数")
public class AppointmentWorkOrderSearchParams extends PageVO {

    @Schema(description = "店铺 ID")
    private String storeId;

    @Schema(description = "店铺名称（运营端展示）")
    private String storeName;

    /**
     * @see cn.lili.modules.appointment.entity.enums.AppointmentWorkOrderStatusEnum
     */
    @Schema(description = "工单状态")
    private String serviceStatus;

    @Schema(description = "工单号")
    private String workOrderSn;

    @Schema(description = "订单编号")
    private String orderSn;

    /**
     * @see cn.lili.modules.appointment.entity.enums.AppointmentServiceModeEnum
     */
    @Schema(description = "服务模式 IN_STORE/ON_SITE")
    private String serviceMode;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @Schema(description = "预约日期起")
    private Date appointmentDateStart;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @Schema(description = "预约日期止")
    private Date appointmentDateEnd;

    @Schema(description = "买家会员 ID")
    private String memberId;
}
