package cn.lili.modules.promotion.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.Data;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * 礼品卡活动创建/编辑请求参数
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@Schema(description = "礼品卡活动保存参数")
public class GiftCardCashActivityDTO {

    @NotBlank(message = "活动名称不能为空")
    @Size(max = 64, message = "活动名称过长")
    @Schema(description = "活动名称/礼品卡名称，最多 64 个字符", requiredMode = Schema.RequiredMode.REQUIRED)
    private String giftCardName;

    @Schema(description = "礼品卡类型：CASH 现金卡、PICKUP 提货卡；创建默认 CASH，创建后不可切换，编辑未传时沿用原类型", allowableValues = {"CASH", "PICKUP"})
    private String cardType;

    @Schema(description = "提货卡兑换方式：ONE 任选一种、ALL 全部兑换、ANY 任选指定种数；每种 SKU 一件，整卡一次使用", allowableValues = {"ONE", "ALL", "ANY"})
    private String pickupMode;

    @Schema(description = "提货卡兑换种数：ANY 模式必填，为 1 至候选商品数；ONE 归一化为 1，ALL 归一化为候选商品数")
    private Integer pickupCount;

    @Schema(description = "提货卡候选 SKU ID 列表，对应 li_goods_sku.id；须为 1～50 个不同的、审核通过且已上架的普通实物 SKU")
    private List<String> pickupSkuIds;

    @Schema(description = "单卡面额（人民币元）；现金卡必填、大于零且最多两位小数，提货卡由服务端固定为零")
    private BigDecimal faceValue;

    @Schema(description = "有效期类型：LONG_TERM 长期有效、FIXED_UNTIL 固定到期、AFTER_ACTIVATE_MONTHS 激活后按月有效；默认 LONG_TERM", allowableValues = {"LONG_TERM", "FIXED_UNTIL", "AFTER_ACTIVATE_MONTHS"})
    private String validityType;

    @Schema(description = "固定到期时间（北京时间）；FIXED_UNTIL 时必填，保存时须晚于当前时间")
    private Date validEndTime;

    @Schema(description = "激活后有效月数（自然月）；AFTER_ACTIVATE_MONTHS 时必填，取值 1～99，由激活时间计算卡到期时间")
    private Integer validMonthsAfterActivate;

    @NotNull(message = "活动库存不能为空")
    @Min(value = 1, message = "活动库存至少为1")
    @Schema(description = "总库存（张），至少为 1；制卡与直接发卡共用，修改时不得小于已占用凭据数", requiredMode = Schema.RequiredMode.REQUIRED)
    private Integer stockTotal;
}
