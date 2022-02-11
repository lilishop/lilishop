package cn.lili.modules.order.order.entity.vo;

import cn.lili.mybatis.BaseEntity;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 订单支付日志：实际为订单部分字段提取过来的一个vo
 *
 * @author Chopper
 * @since 2020/11/17 7:30 下午
 */
@Data

@ApiModel(value = "订单")
public class PaymentLog extends BaseEntity {


    private static final long serialVersionUID = 2233811628066468683L;
    @ApiModelProperty("订单编号")
    private String sn;

    @ApiModelProperty("交易编号 关联Trade")
    private String tradeSn;

    @ApiModelProperty(value = "店铺ID")
    private String storeId;

    @ApiModelProperty(value = "店铺名称")
    private String storeName;

    @ApiModelProperty(value = "会员ID")
    private String memberId;

    @ApiModelProperty(value = "用户名")
    private String memberName;

    /**
     * @see PayStatusEnum
     */
    @ApiModelProperty(value = "付款状态")
    private String payStatus;

    @ApiModelProperty(value = "第三方付款流水号")
    private String receivableNo;

    @ApiModelProperty(value = "支付方式")
    private String paymentMethod;

    @ApiModelProperty(value = "支付时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date paymentTime;


    @ApiModelProperty(value = "总价格")
    private Double flowPrice;

    @ApiModelProperty(value = "支付方式返回的交易号")
    private String payOrderNo;

    /**
     * @see ClientTypeEnum
     */
    @ApiModelProperty(value = "订单来源")
    private String clientType;

    /**
     * @see OrderTypeEnum
     */
    @ApiModelProperty(value = "订单类型")
    private String orderType;

}