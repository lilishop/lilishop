package cn.lili.modules.payment.kit.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import lombok.ToString;

/**
 * 支付参数
 *
 * @author Chopper
 * @since 2020/12/19 11:46
 */
@Data
@ToString
public class PayParam {


    @NotNull
    @Schema(description = "交易类型", allowableValues = "TRADE,ORDER,RECHARGE,PAID_MEMBER")
    private String orderType;

    @NotNull
    @Schema(description = "订单号")
    private String sn;

    @NotNull
    @Schema(description = "客户端类型")
    private String clientType;


}
