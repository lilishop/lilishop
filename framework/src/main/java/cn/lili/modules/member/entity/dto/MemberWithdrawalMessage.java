package cn.lili.modules.member.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 会员提现消息
 *
 * @author Bulbasaur
 * @since 2020/12/14 16:31
 */
@Data
public class MemberWithdrawalMessage {

    @ApiModelProperty(value = "金额")
    private Double price;

    @ApiModelProperty(value = "会员id")
    private String memberId;

    @ApiModelProperty(value = "提现状态")
    private String status;

    /**
     * @see cn.lili.modules.member.entity.enums.MemberWithdrawalDestinationEnum
     */
    @ApiModelProperty(value = "提现到哪里")
    private String destination;
}
