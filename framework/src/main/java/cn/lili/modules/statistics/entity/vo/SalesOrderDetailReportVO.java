package cn.lili.modules.statistics.entity.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 销售订单明细报表
 *
 * @author Bulbasaur
 * @since 2026/07/20
 */
@Data
@Schema(description = "销售订单明细报表")
public class SalesOrderDetailReportVO {

    @Schema(description = "订单编号")
    private String orderSn;

    @Schema(description = "售后单号")
    private String refundSn;

    @Schema(description = "订单编号|售后单号")
    private String orderRefundSn;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "发生时间")
    private Date occurTime;

    @Schema(description = "交易类型")
    private String flowType;

    @Schema(description = "交易类型名称")
    private String flowTypeName;

    @Schema(description = "成交数量")
    private Integer num;

    @Schema(description = "成交金额")
    private Double transactionAmount;

    @Schema(description = "售价金额")
    private Double salePriceAmount;

    @Schema(description = "优惠金额")
    private Double discountAmount;

    @Schema(description = "收款明细")
    private String paymentDetail;

    @Schema(description = "优惠明细")
    private String discountDetail;

    @Schema(description = "订单来源")
    private String clientType;

    @Schema(description = "支付方式编码")
    private String paymentName;

    @Schema(description = "平台优惠券金额")
    private Double siteCouponPrice;

    @Schema(description = "限时直降补贴")
    private Double flashDiscountSubsidy;

    @Schema(description = "第N件优惠补贴")
    private Double nthItemSubsidy;
}
