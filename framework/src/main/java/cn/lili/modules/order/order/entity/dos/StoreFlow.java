package cn.lili.modules.order.order.entity.dos;

import cn.lili.modules.order.order.entity.enums.FlowTypeEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 商家订单流水
 *
 * @author Chopper
 * @since 2020/11/17 7:31 下午
 */
@Data
@TableName("li_store_flow")
@ApiModel(value = "商家订单流水")
public class StoreFlow extends BaseIdEntity {

    private static final long serialVersionUID = -5998757398902747939L;

    @ApiModelProperty(value = "流水编号")
    private String sn;

    @ApiModelProperty(value = "订单sn")
    private String orderSn;

    @ApiModelProperty(value = "子订单sn")
    private String orderItemSn;

    @ApiModelProperty(value = "售后SN")
    private String refundSn;

    @ApiModelProperty(value = "店铺id")
    private String storeId;

    @ApiModelProperty(value = "店铺名称 ")
    private String storeName;

    @ApiModelProperty(value = "会员id")
    private String memberId;

    @ApiModelProperty(value = "会员名称")
    private String memberName;


    @ApiModelProperty(value = "商品ID")
    private String goodsId;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "货品ID")
    private String skuId;

    @ApiModelProperty(value = "图片")
    private String image;

    @ApiModelProperty(value = "分类ID")
    private String categoryId;

    @ApiModelProperty(value = "规格json")
    private String specs;


    /**
     * @see FlowTypeEnum
     */
    @ApiModelProperty(value = "流水类型：PAY/REFUND 支付/退款", allowableValues = "PAY,REFUND")
    private String flowType;

    /**
     * @see cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum
     */
    @ApiModelProperty(value = "订单促销类型")
    private String orderPromotionType;

    @ApiModelProperty(value = "积分活动商品结算价格")
    private Double pointSettlementPrice;

    @ApiModelProperty(value = "砍价活动商品结算价格")
    private Double kanjiaSettlementPrice;

    @ApiModelProperty(value = "平台优惠券 使用金额")
    private Double siteCouponPrice;

    @ApiModelProperty(value = "站点优惠券补贴比例")
    private Double siteCouponPoint;

    @ApiModelProperty(value = "站点优惠券补贴金额")
    private Double siteCouponCommission;

    @ApiModelProperty(value = "单品分销返现支出")
    private Double distributionRebate;

    @ApiModelProperty(value = "平台收取交易佣金")
    private Double commissionPrice;

    @ApiModelProperty(value = "流水金额")
    private Double finalPrice;

    @ApiModelProperty(value = "最终结算金额")
    private Double billPrice;

    @ApiModelProperty(value = "第三方交易流水号")
    private String transactionId;

    /**
     * @see  PaymentMethodEnum
     */
    @ApiModelProperty(value = "支付方式名称")
    private String paymentName;

    @ApiModelProperty(value = "销售量")
    private Integer num;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;
}