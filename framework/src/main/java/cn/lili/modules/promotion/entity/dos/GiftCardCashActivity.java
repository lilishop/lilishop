package cn.lili.modules.promotion.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 礼品卡活动实体类（库存与规则；其下可多次制卡、多次发卡/发送）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@TableName("li_gift_card_cash_activity")
@Schema(description = "礼品卡活动（现金卡、提货卡）")
public class GiftCardCashActivity extends BaseEntity {

    @Schema(description = "活动名称，同时作为礼品卡名称展示")
    private String giftCardName;

    @Schema(description = "礼品卡类型：CASH 现金卡、PICKUP 提货卡；创建后不可切换", allowableValues = {"CASH", "PICKUP"})
    private String cardType;

    @Schema(description = "提货卡兑换方式：ONE 任选一种、ALL 全部兑换、ANY 任选指定种数；每种 SKU 一件，整卡一次使用", allowableValues = {"ONE", "ALL", "ANY"})
    private String pickupMode;

    @Schema(description = "提货卡兑换的不同 SKU 种数：ANY 为 1 至候选商品数，ONE 固定 1，ALL 为候选商品数；现金卡为空")
    private Integer pickupCount;

    @Schema(description = "提货卡候选 SKU ID 的 JSON 字符串数组，对应 li_goods_sku.id；包含 1～50 个不同 SKU，制发卡后不可修改")
    private String pickupSkuIds;

    @Schema(description = "单卡面额（人民币元），保留两位小数；现金卡大于零，提货卡固定为零")
    private BigDecimal faceValue;

    @Schema(description = "有效期类型：LONG_TERM 长期有效、FIXED_UNTIL 固定到期、AFTER_ACTIVATE_MONTHS 激活后按月有效", allowableValues = {"LONG_TERM", "FIXED_UNTIL", "AFTER_ACTIVATE_MONTHS"})
    private String validityType;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "固定到期时间，展示及解析时区为 GMT+8；仅 FIXED_UNTIL 模式使用")
    private Date validEndTime;

    @Schema(description = "激活后有效月数（自然月），取值 1～99；仅 AFTER_ACTIVATE_MONTHS 模式使用，由激活时间计算卡到期时间")
    private Integer validMonthsAfterActivate;

    @Schema(description = "活动总库存（张），制卡与直接发卡共用的凭据数量上限")
    private Integer stockTotal;

}
