package cn.lili.modules.appointment.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 预约核销结果
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@Schema(description = "预约核销结果")
public class AppointmentVerificationResultVO implements Serializable {

    @Schema(description = "订单编号")
    private String orderSn;

    @Schema(description = "核销码")
    private String code;

    @Schema(description = "序号")
    private Integer seqNo;

    @Schema(description = "核销码总数")
    private Integer totalCount;

    @Schema(description = "已核销数量")
    private Integer verifiedCount;

    @Schema(description = "是否全部核销")
    private Boolean allVerified;

    @Schema(description = "订单是否已完成")
    private Boolean orderCompleted;

    @Schema(description = "工单ID")
    private String workOrderId;

    @Schema(description = "工单编号")
    private String workOrderSn;

    @Schema(description = "工单服务状态")
    private String workOrderStatus;

    @Schema(description = "服务方式 IN_STORE/ON_SITE")
    private String serviceMode;

    @Schema(description = "是否展示工单")
    private Boolean workOrderVisible;
}
