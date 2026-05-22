package cn.lili.modules.store.entity.dos;

import cn.lili.modules.store.entity.enums.BillStatusEnum;
import cn.lili.mybatis.BaseEntity;
import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
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
@EqualsAndHashCode(callSuper = false)
@TableName("li_bill")
@Schema(description = "结算单")
public class Bill extends BaseIdEntity {

    private static final long serialVersionUID = 1L;


    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @Schema(description = "创建时间", hidden = true)
    private Date createTime;

    @Schema(description = "账单号")
    private String sn;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @Schema(description = "结算开始时间")
    private Date startTime;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @Schema(description = "结算结束时间")
    private Date endTime;


    /**
     * @see BillStatusEnum
     */
    @Schema(description = "状态：OUT(已出账),CHECK(已对账),EXAMINE(已审核),PAY(已付款)")
    private String billStatus;

    @Schema(description = "店铺id")
    private String storeId;

    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "平台付款时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(iso = DateTimeFormat.ISO.DATE_TIME)
    private Date payTime;

    @Schema(description = "银行开户名")
    private String bankAccountName;

    @Schema(description = "公司银行账号")
    private String bankAccountNumber;

    @Schema(description = "开户银行支行名称")
    private String bankName;

    @Schema(description = "支行联行号")
    private String bankCode;

    @Schema(description = "结算周期内订单付款总金额")
    private Double orderPrice;

    @Schema(description = "退单金额")
    private Double refundPrice;

    @Schema(description = "平台收取佣金")
    private Double commissionPrice;

    @Schema(description = "退单产生退还佣金金额")
    private Double refundCommissionPrice;

    @Schema(description = "分销返现支出")
    private Double distributionCommission;

    @Schema(description = "分销订单退还，返现佣金返还")
    private Double distributionRefundCommission;

    @Schema(description = "平台优惠券补贴")
    private Double siteCouponCommission;

    @Schema(description = "退货平台优惠券补贴返还")
    private Double siteCouponRefundCommission;

    @Schema(description = "积分商品结算价格")
    private Double pointSettlementPrice;

    @Schema(description = "退货积分补贴返还")
    private Double pointRefundSettlementPrice;

    @Schema(description = "砍价商品结算价格")
    private Double kanjiaSettlementPrice;

    @Schema(description = "退货砍价补贴返还")
    private Double kanjiaRefundSettlementPrice;

    @Schema(description = "礼品卡平台补贴（结算周期内）")
    private Double giftCardSubsidy;

    @Schema(description = "退货礼品卡平台补贴返还")
    private Double giftCardRefundSubsidy;


    /**
     * 开始算钱啦
     * billPrice(最终结算金额) =
     * orderPrice(结算周期内订单付款总金额) - refundPrice(退单金额)
     * - commissionPrice(平台收取佣金) + refundCommissionPrice(退单产生退还佣金金额)
     * - distributionCommission(分销返现支出) + distributionRefundCommission(分销订单退还，返现佣金返还)
     * + siteCouponCommission(平台优惠券补贴) - siteCouponRefundCommission(退货平台优惠券补贴返还)
     * + giftCardSubsidy(礼品卡平台补贴) - giftCardRefundSubsidy(退货礼品卡补贴返还)
     * + kanjiaSettlementPrice(砍价商品结算价格) + pointSettlementPrice(pointSettlementPrice)
     */
    @Schema(description = "最终结算金额")
    private Double billPrice;


}