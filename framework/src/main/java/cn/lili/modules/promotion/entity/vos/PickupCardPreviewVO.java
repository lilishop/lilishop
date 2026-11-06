package cn.lili.modules.promotion.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 提货卡对外试算结果，仅返回买家需确认的数量与实付金额，不暴露内部商品成本及结算数据。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
@Data
@Schema(description = "提货卡兑换试算结果")
public class PickupCardPreviewVO {

    @Schema(description = "当前兑换不同 SKU 的种数，每种一件")
    private Integer skuCount;

    @Schema(description = "买家现金应付，单位元，成功校验的提货兑换固定为零")
    private BigDecimal cashPayAmount;

    @Schema(description = "买家应付运费，单位元，提货兑换固定为零")
    private BigDecimal freightAmount;
}
