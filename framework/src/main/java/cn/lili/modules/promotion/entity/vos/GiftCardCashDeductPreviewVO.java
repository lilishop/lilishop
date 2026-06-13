package cn.lili.modules.promotion.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * 礼品卡抵扣试算结果展示对象
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@Schema(description = "礼品卡抵扣试算结果")
public class GiftCardCashDeductPreviewVO {

    @Schema(description = "订单应付总金额")
    private BigDecimal orderAmount;

    @Schema(description = "礼品卡抵扣总金额")
    private BigDecimal giftCardDeductAmount;

    @Schema(description = "现金差额应付金额")
    private BigDecimal cashPayAmount;

    @Schema(description = "参与抵扣的礼品卡明细")
    private List<DeductItem> deductItems = new ArrayList<>();

    @Data
    @Schema(description = "礼品卡抵扣明细")
    public static class DeductItem {
        @Schema(description = "礼品卡ID（凭证ID）")
        private String credentialId;
        @Schema(description = "礼品卡卡号")
        private String cardNo;
        @Schema(description = "本次抵扣金额")
        private BigDecimal deductAmount;
    }
}
