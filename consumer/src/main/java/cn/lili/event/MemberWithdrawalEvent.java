package cn.lili.event;

import cn.lili.modules.member.entity.dto.MemberWithdrawalMessage;

/**
 * 会员提现消息
 *
 * @author Chopper
 * @date 2020/11/17 7:13 下午
 */
public interface MemberWithdrawalEvent {

    /**
     * 会员提现
     *
     * @param memberWithdrawalMessage 提现对象
     */
    void memberWithdrawal(MemberWithdrawalMessage memberWithdrawalMessage);
}
