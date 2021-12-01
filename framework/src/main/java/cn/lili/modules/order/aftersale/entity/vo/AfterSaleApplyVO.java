package cn.lili.modules.order.aftersale.entity.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 售后申请VO
 *
 * @author Chopper
 * @since 2021/3/12 10:33 上午
 */
@Data
public class AfterSaleApplyVO {

    @ApiModelProperty(value = "申请退款金额单价")
    private Double applyRefundPrice;

    @ApiModelProperty(value = "可申请数量")
    private Integer num;

    @ApiModelProperty(value = "订单子项编号")
    private String orderItemSn;

    @ApiModelProperty(value = "商品ID")
    private String goodsId;

    @ApiModelProperty(value = "货品ID")
    private String skuId;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "商品图片")
    private String image;

    @ApiModelProperty(value = "商品价格")
    private Double goodsPrice;

    /**
     * @see cn.lili.modules.order.trade.entity.enums.AfterSaleRefundWayEnum
     */
    @ApiModelProperty(value = "退款方式", allowableValues = "ORIGINAL,OFFLINE")
    private String refundWay;

    /**
     * @see cn.lili.modules.order.trade.entity.enums
     */
    @ApiModelProperty(value = "账号类型", allowableValues = "ALIPAY,WECHATPAY,MEMBERWALLET,BANKTRANSFER")
    private String accountType;

    @ApiModelProperty(value = "是否支持退货")
    private Boolean returnGoods;

    @ApiModelProperty(value = "是否支持退款")
    private Boolean returnMoney;

    @ApiModelProperty(value = "会员ID")
    private String memberId;




}
