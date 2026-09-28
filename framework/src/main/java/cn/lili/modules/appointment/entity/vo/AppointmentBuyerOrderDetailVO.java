package cn.lili.modules.appointment.entity.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * 买家预约订单详情
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@Schema(description = "买家预约订单详情")
public class AppointmentBuyerOrderDetailVO implements Serializable {

    @Schema(description = "订单编号")
    private String orderSn;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "订单状态")
    private String orderStatus;

    @Schema(description = "服务方式 IN_STORE/ON_SITE")
    private String serviceMode;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @Schema(description = "预约日期")
    private Date slotDate;

    @Schema(description = "时段开始")
    private String slotStartTime;

    @Schema(description = "时段结束")
    private String slotEndTime;

    @Schema(description = "预约数量")
    private Integer quantity;

    @Schema(description = "联系人")
    private String contactName;

    @Schema(description = "联系电话")
    private String contactPhone;

    @Schema(description = "上门服务地址快照")
    private String serviceAddressSnapshot;

    @Schema(description = "补充信息答案 JSON")
    private String formAnswerJson;

    @Schema(description = "取消规则快照")
    private String cancelPolicySnapshot;

    @Schema(description = "工单服务状态")
    private String workOrderStatus;

    @Schema(description = "核销码列表")
    private List<AppointmentVerificationCodeVO> codes;
}
