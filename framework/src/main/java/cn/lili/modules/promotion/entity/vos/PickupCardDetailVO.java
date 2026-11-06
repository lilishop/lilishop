package cn.lili.modules.promotion.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

/**
 * 当前会员提货卡兑换详情，包含固定兑换规则及实时商品可用性。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
@Data
@Schema(description = "会员提货卡兑换详情")
public class PickupCardDetailVO {

    @Schema(description = "提货卡凭证 ID，关联 li_gift_card_cash_credential")
    private String credentialId;

    @Schema(description = "礼品卡名称")
    private String giftCardName;

    @Schema(description = "兑换方式：ONE 任选一、ALL 全选、ANY 任选指定种数")
    private String pickupMode;

    @Schema(description = "一次必须兑换的不同 SKU 数，每种一件")
    private Integer pickupCount;

    @Schema(description = "候选商品；已下架、删除或售罄仍展示但不可提交")
    private List<PickupCardSkuVO> goods;

    @Schema(description = "已成功兑换的交易号，关联 li_trade.sn；取消也不清空")
    private String tradeSn;
}
