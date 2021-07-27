package cn.lili.modules.payment.kit.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.ToString;

import javax.validation.constraints.NotNull;

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
    @ApiModelProperty(value = "交易类型", allowableValues = "TRADE,ORDER,RECHARGE")
    private String orderType;

    @NotNull
    @ApiModelProperty(value = "订单号")
    private String sn;

    @NotNull
    @ApiModelProperty(value = "客户端类型")
    private String clientType;



}
