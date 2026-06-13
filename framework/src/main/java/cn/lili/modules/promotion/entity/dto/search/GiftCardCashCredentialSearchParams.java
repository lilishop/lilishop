package cn.lili.modules.promotion.entity.dto.search;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 卡密凭证分页查询条件（平台库存）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@Schema(description = "卡密凭证分页查询（平台库存）")
public class GiftCardCashCredentialSearchParams {

    @Schema(description = "活动ID")
    private String activityId;

    @Schema(description = "制卡批次ID")
    private String createBatchId;

    @Schema(description = "发卡批次ID")
    private String issueBatchId;

    @Schema(description = "卡号（模糊）")
    private String cardNo;

    @Schema(description = "会员手机号（精确）")
    private String memberMobile;

    @Schema(description = "状态 UNISSUED/BOUND/VOIDED/EXPIRED")
    private String status;
}
