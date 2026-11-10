package cn.lili.modules.promotion.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

/**
 * 满额活动（满减 / 满折 / 满赠）单级阶梯规则 DTO
 * <p>
 * 每个满额活动可配置 1~3 级阶梯，按门槛金额 {@link #fullMoney} 升序排列。
 * 买家结算时仅触发满足门槛的最高一级阶梯：
 * <ul>
 *     <li>满减活动（FULL_MINUS）：使用 {@link #fullMoney} 与 {@link #fullMinus}</li>
 *     <li>满折活动（FULL_RATE）：使用 {@link #fullMoney} 与 {@link #fullRate}</li>
 *     <li>满赠活动（FULL_GIFT）：使用 {@link #fullMoney} 与 {@link #giftItems}</li>
 * </ul>
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "满额活动阶梯规则项")
public class FullDiscountRuleDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "优惠门槛金额（元）")
    private Double fullMoney;

    @Schema(description = "减现金金额（元，仅满减活动有效）")
    private Double fullMinus;

    @Schema(description = "优惠折扣（0.1~9.9折，仅满折活动有效）")
    private Double fullRate;

    @Schema(description = "满赠赠品列表（每阶梯最多3个赠品，每个1~10件）")
    private List<FullDiscountGiftItem> giftItems;
}
