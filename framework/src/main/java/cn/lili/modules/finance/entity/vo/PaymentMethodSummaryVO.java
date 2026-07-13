package cn.lili.modules.finance.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 支付方式汇总
 */
@Data
@Schema(description = "支付方式汇总")
public class PaymentMethodSummaryVO {

    @Schema(description = "日期")
    private String period;

    @Schema(description = "支付方式")
    private String paymentName;

    @Schema(description = "支付笔数")
    private Long payCount;

    @Schema(description = "支付金额")
    private Double payAmount;

    @Schema(description = "退款笔数")
    private Long refundCount;

    @Schema(description = "退款金额")
    private Double refundAmount;
}
