package cn.lili.event;

import cn.lili.modules.member.entity.dto.MemberPointMessage;

/**
 * 会员积分改变消息
 *
 * @author Chopper
 * @since 2020/11/17 7:13 下午
 */
public interface MemberPointChangeEvent {

    /**
     * 会员积分改变消息
     *
     * @param memberPointMessage 会员积分消息
     */
    void memberPointChange(MemberPointMessage memberPointMessage);
}
