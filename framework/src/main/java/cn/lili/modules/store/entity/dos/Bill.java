package cn.lili.modules.store.entity.dos;

import cn.lili.modules.store.entity.enums.BillStatusEnum;
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
 * 结算单
 *
 * @author Chopper
 * @since 2020/11/17 4:27 下午
 */
@Data
@TableName("li_bill")
@ApiModel(value = "结算单")
public class Bill extends BaseIdEntity {

    private static final long serialVersionUID = 1L;


    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;

    @ApiModelProperty(value = "账单号")
    private String sn;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @ApiModelProperty(value = "结算开始时间")
    private Date startTime;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @ApiModelProperty(value = "结算结束时间")
    private Date endTime;


    /**
     * @see BillStatusEnum
     */
    @ApiModelProperty(value = "状态：OUT(已出账),CHECK(已对账),EXAMINE(已审核),PAY(已付款)")
    private String billStatus;

    @ApiModelProperty(value = "店铺id")
    private String storeId;

    @ApiModelProperty(value = "店铺名称")
    private String storeName;

    @ApiModelProperty(value = "平台付款时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
    private Date payTime;

    @ApiModelProperty(value = "银行开户名")
    private String bankAccountName;

    @ApiModelProperty(value = "公司银行账号")
    private String bankAccountNumber;

    @ApiModelProperty(value = "开户银行支行名称")
    private String bankName;

    @ApiModelProperty(value = "支行联行号")
    private String bankCode;

    @ApiModelProperty(value = "结算周期内订单付款总金额")
    private Double orderPrice;

    @ApiModelProperty(value = "退单金额")
    private Double refundPrice;

    @ApiModelProperty(value = "平台收取佣金")
    private Double commissionPrice;

    @ApiModelProperty(value = "退单产生退还佣金金额")
    private Double refundCommissionPrice;

    @ApiModelProperty(value = "分销返现支出")
    private Double distributionCommission;

    @ApiModelProperty(value = "分销订单退还，返现佣金返还")
    private Double distributionRefundCommission;

    @ApiModelProperty(value = "平台优惠券补贴")
    private Double siteCouponCommission;

    @ApiModelProperty(value = "退货平台优惠券补贴返还")
    private Double siteCouponRefundCommission;

    @ApiModelProperty(value = "积分商品结算价格")
    private Double pointSettlementPrice;

    @ApiModelProperty(value = "砍价商品结算价格")
    private Double kanjiaSettlementPrice;


    /**
     * 开始算钱啦
     * billPrice(最终结算金额) =
     *   orderPrice(结算周期内订单付款总金额) - refundPrice(退单金额)
     * - commissionPrice(平台收取佣金) + refundCommissionPrice(退单产生退还佣金金额)
     * - distributionCommission(分销返现支出) + distributionRefundCommission(分销订单退还，返现佣金返还)
     * + siteCouponCommission(平台优惠券补贴) - siteCouponRefundCommission(退货平台优惠券补贴返还)
     * + kanjiaSettlementPrice(砍价商品结算价格) + pointSettlementPrice(pointSettlementPrice)
     */
    @ApiModelProperty(value = "最终结算金额")
    private Double billPrice;


}