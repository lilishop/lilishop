package cn.lili.modules.wallet.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 客户提现消息
 *
 * @author Bulbasaur
 * @since 2020/12/14 16:31
 */
@Data
public class MemberWithdrawalMessage {

    @Schema(description = "提现申请ID")
    private String memberWithdrawApplyId;

    @Schema(description = "金额")
    private Double price;

    @Schema(description = "客户id")
    private String memberId;

    @Schema(description = "提现状态")
    private String status;

    @Schema(description = "延迟重试次数")
    private Integer retryCount;

}
