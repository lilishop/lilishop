package cn.lili.modules.payment.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Date;

/**
 * 退款日志
 *
 * @author Chopper
 * @date 2021/1/28 09:21
 */
@Data
@Entity
@Table(name = "li_refund_log")
@TableName("li_refund_log")
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ApiModel(value = "退款日志")
public class RefundLog {

    @Id
    @TableId
    @TableField
    @Column(columnDefinition = "bigint(20)")
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

    @ApiModelProperty(value = "会员ID")
    private String memberId;

    @ApiModelProperty(value = "退单编号")
    private String afterSaleNo;

    @ApiModelProperty(value = "订单编号")
    private String orderSn;

    @ApiModelProperty(value = "金额")
    private Double totalAmount;

    @ApiModelProperty(value = "改笔交易支付金额")
    private Double payPrice;

    @ApiModelProperty(value = "是否已退款")
    private Boolean isRefund ;

    @ApiModelProperty(value = "退款方式")
    private String paymentName;


    @ApiModelProperty(value = "支付第三方付款流水")
    private String paymentReceivableNo;

    @ApiModelProperty(value = "退款请求流水")
    private String outOrderNo;


    @ApiModelProperty(value = "第三方退款流水号")
    private String receivableNo;

    @ApiModelProperty(value = "退款理由")
    private String refundReason;

    @ApiModelProperty(value = "退款失败原因")
    private String errorMessage;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;
}