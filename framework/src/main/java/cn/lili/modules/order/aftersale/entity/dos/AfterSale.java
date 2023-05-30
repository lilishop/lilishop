package cn.lili.modules.order.aftersale.entity.dos;

import cn.lili.common.security.sensitive.Sensitive;
import cn.lili.common.security.sensitive.enums.SensitiveStrategy;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 售后
 *
 * @author Chopper
 * @since 2020/11/17 7:30 下午
 */
@Data
@TableName("li_after_sale")
@ApiModel(value = "售后")
public class AfterSale extends BaseEntity {

    private static final long serialVersionUID = -5339221840646353416L;

    //基础信息

    @ApiModelProperty(value = "售后服务单号")
    private String sn;

    @ApiModelProperty(value = "订单编号")
    private String orderSn;

    @ApiModelProperty(value = "订单货物编号")
    private String orderItemSn;

    @ApiModelProperty(value = "交易编号")
    private String tradeSn;

    @ApiModelProperty(value = "会员ID")
    private String memberId;

    @ApiModelProperty(value = "会员名称")
    @Sensitive(strategy = SensitiveStrategy.PHONE)
    private String memberName;

    @ApiModelProperty(value = "商家ID")
    private String storeId;

    @ApiModelProperty(value = "商家名称")
    private String storeName;

    //商品信息

    @ApiModelProperty(value = "商品ID")
    private String goodsId;
    @ApiModelProperty(value = "货品ID")
    private String skuId;
    @ApiModelProperty(value = "申请数量")
    private Integer num;
    @ApiModelProperty(value = "商品图片")
    private String goodsImage;
    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "规格json")
    private String specs;
    @ApiModelProperty(value = "实际金额")
    private Double flowPrice;


    //交涉信息

    @ApiModelProperty(value = "申请原因")
    private String reason;

    @ApiModelProperty(value = "问题描述")
    private String problemDesc;

    @ApiModelProperty(value = "评价图片")
    private String afterSaleImage;

    /**
     * @see cn.lili.modules.order.trade.entity.enums.AfterSaleTypeEnum
     */
    @ApiModelProperty(value = "售后类型", allowableValues = "RETURN_GOODS,RETURN_MONEY")
    private String serviceType;

    /**
     * @see cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum
     */
    @ApiModelProperty(value = "售后单状态", allowableValues = "APPLY,PASS,REFUSE,BUYER_RETURN,SELLER_RE_DELIVERY,BUYER_CONFIRM,SELLER_CONFIRM,COMPLETE")
    private String serviceStatus;

    //退款信息

    /**
     * @see cn.lili.modules.order.trade.entity.enums.AfterSaleRefundWayEnum
     */
    @ApiModelProperty(value = "退款方式", allowableValues = "ORIGINAL,OFFLINE")
    private String refundWay;

    @ApiModelProperty(value = "账号类型", allowableValues = "ALIPAY,WECHATPAY,BANKTRANSFER")
    private String accountType;

    @ApiModelProperty(value = "银行账户")
    private String bankAccountNumber;

    @ApiModelProperty(value = "银行开户名")
    private String bankAccountName;

    @ApiModelProperty(value = "银行开户行")
    private String bankDepositName;

    @ApiModelProperty(value = "商家备注")
    private String auditRemark;

    @ApiModelProperty(value = "订单支付方式返回的交易号")
    private String payOrderNo;

    @ApiModelProperty(value = "申请退款金额")
    private Double applyRefundPrice;

    @ApiModelProperty(value = "实际退款金额")
    private Double actualRefundPrice;

    @ApiModelProperty(value = "退还积分")
    private Integer refundPoint;

    @ApiModelProperty(value = "退款时间")
    private Date refundTime;

    /**
     * 买家物流信息
     */
    @ApiModelProperty(value = "发货单号")
    private String mLogisticsNo;

    @ApiModelProperty(value = "物流公司CODE")
    private String mLogisticsCode;

    @ApiModelProperty(value = "物流公司名称")
    private String mLogisticsName;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @ApiModelProperty(value = "买家发货时间")
    private Date mDeliverTime;

}