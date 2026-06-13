package cn.lili.modules.promotion.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.DecimalMin;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

/**
 * 礼品卡抵扣试算请求参数
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@Schema(description = "礼品卡抵扣试算参数")
public class GiftCardCashDeductPreviewDTO {

    @Schema(description = "选中的礼品卡ID列表（凭证ID）")
    private List<String> credentialIds;

    @DecimalMin(value = "0.00", message = "订单金额不能小于0")
    @Schema(description = "订单应付总金额", requiredMode = Schema.RequiredMode.REQUIRED)
    private BigDecimal orderAmount;
}
