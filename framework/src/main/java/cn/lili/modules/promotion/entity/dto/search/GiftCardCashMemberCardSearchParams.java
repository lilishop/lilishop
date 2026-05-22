package cn.lili.modules.promotion.entity.dto.search;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 会员礼品卡分页查询条件
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@Schema(description = "会员礼品卡分页查询")
public class GiftCardCashMemberCardSearchParams {

    @Schema(description = "会员礼品卡状态：AVAILABLE/UNAVAILABLE/PENDING_ACTIVATION")
    private String memberCardStatus;
}
