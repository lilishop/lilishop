package cn.lili.modules.order.aftersale.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 售后dto
 *
 * @author Chopper
 * @since 2021/3/20 10:48
 */
@Data
public class AfterSaleDTO {

    @ApiModelProperty(value = "订单SN")
    private String orderItemSn;

    @ApiModelProperty(value = "商品ID")
    private String goodsId;

    @ApiModelProperty(value = "货品ID")
    private String skuId;

    @ApiModelProperty(value = "申请退款金额")
    private Double applyRefundPrice;

    @ApiModelProperty(value = "申请数量")
    private Integer num;

    @ApiModelProperty(value = "申请原因")
    private String reason;

    @ApiModelProperty(value = "问题描述")
    private String problemDesc;

    @ApiModelProperty(value = "售后图片")
    private String images;

    /**
     * @see cn.lili.modules.order.trade.entity.enums.AfterSaleTypeEnum
     */
    @ApiModelProperty(value = "售后类型", allowableValues = "RETURN_GOODS,EXCHANGE_GOODS,RETURN_MONEY")
    private String serviceType;

    /**
     * @see cn.lili.modules.order.trade.entity.enums.AfterSaleRefundWayEnum
     */
    @ApiModelProperty(value = "退款方式", allowableValues = "ORIGINAL,OFFLINE")
    private String refundWay;

    @ApiModelProperty(value = "账号类型", allowableValues = "ALIPAY,WECHATPAY,BANKTRANSFER")
    private String accountType;

    @ApiModelProperty(value = "银行开户行")
    private String bankDepositName;

    @ApiModelProperty(value = "银行开户名")
    private String bankAccountName;

    @ApiModelProperty(value = "银行卡号")
    private String bankAccountNumber;
}
