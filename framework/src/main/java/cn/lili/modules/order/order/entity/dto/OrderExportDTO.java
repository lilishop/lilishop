package cn.lili.modules.order.order.entity.dto;

import cn.lili.modules.order.order.entity.enums.DeliverStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 订单导出DTO
 * @author Bulbasaur
 * @since 2021/6/3 6:36 下午
 *
 */
@Data
public class OrderExportDTO {

    @ApiModelProperty("订单编号")
    private String sn;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "创建时间")
    private Date createTime;

    @ApiModelProperty(value = "用户名")
    private String memberName;

    @ApiModelProperty(value = "收件人姓名")
    private String consigneeName;

    @ApiModelProperty(value = "收件人手机")
    private String consigneeMobile;

    @ApiModelProperty(value = "收件人地址")
    private String consigneeAddressPath;

    @ApiModelProperty(value = "详细地址")
    private String consigneeDetail;

    @ApiModelProperty(value = "支付方式")
    private String paymentMethod;

    @ApiModelProperty(value = "物流公司名称")
    private String logisticsName;

    @ApiModelProperty(value = "运费")
    private Double freightPrice;

    @ApiModelProperty(value = "商品价格")
    private Double goodsPrice;

    @ApiModelProperty(value = "优惠的金额")
    private Double discountPrice;

    @ApiModelProperty(value = "总价格")
    private Double flowPrice;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "商品数量")
    private Integer num;

    @ApiModelProperty(value = "买家订单备注")
    private String remark;

    /**
     * @see OrderStatusEnum
     */
    @ApiModelProperty(value = "订单状态")
    private String orderStatus;

    /**
     * @see PayStatusEnum
     */
    @ApiModelProperty(value = "付款状态")
    private String payStatus;

    /**
     * @see DeliverStatusEnum
     */
    @ApiModelProperty(value = "货运状态")
    private String deliverStatus;

    @ApiModelProperty(value = "是否需要发票")
    private Boolean needReceipt;

    @ApiModelProperty(value = "店铺名称")
    private String storeName;
}
