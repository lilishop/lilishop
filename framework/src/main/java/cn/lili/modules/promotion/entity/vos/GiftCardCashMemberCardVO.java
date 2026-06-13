package cn.lili.modules.promotion.entity.vos;

import cn.lili.modules.promotion.entity.dos.GiftCardCashCredential;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 会员礼品卡分页展示对象（含活动名称与可用状态）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "会员礼品卡分页VO")
public class GiftCardCashMemberCardVO extends GiftCardCashCredential {

    @Schema(description = "礼品卡名称")
    private String giftCardName;

    @Schema(description = "会员礼品卡状态：AVAILABLE/UNAVAILABLE/PENDING_ACTIVATION")
    private String memberCardStatus;
}
