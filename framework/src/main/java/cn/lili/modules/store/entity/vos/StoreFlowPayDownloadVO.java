package cn.lili.modules.store.entity.vos;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 店铺流水下载
 * @author Bulbasaur
 * @date: 2021/8/13 4:14 下午
 *
 */
@Data
public class StoreFlowPayDownloadVO {

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @Schema(description = "创建时间", hidden = true)
    private Date createTime;

    @Schema(description = "订单sn")
    private String orderSn;

    @Schema(description = "店铺名称 ")
    private String storeName;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "销售量")
    private Integer num;

    @Schema(description = "流水金额（用户实付现金）")
    private Double finalPrice;

    @Schema(description = "礼品卡平台补贴金额")
    private Double giftCardSubsidyPrice;

    @Schema(description = "商家结算计费基数")
    private Double settlementBasePrice;

    @Schema(description = "平台收取交易佣金")
    private Double commissionPrice;

    @Schema(description = "平台优惠券 使用金额")
    private Double siteCouponPrice;

    @Schema(description = "单品分销返现支出")
    private Double distributionRebate;

    @Schema(description = "积分活动商品结算价格")
    private Double pointSettlementPrice;

    @Schema(description = "砍价活动商品结算价格")
    private Double kanjiaSettlementPrice;

    @Schema(description = "最终结算金额")
    private Double billPrice;

    @Schema(description = "支付方式")
    private String paymentName;

    @Schema(description = "第三方交易流水号")
    private String transactionId;
}
