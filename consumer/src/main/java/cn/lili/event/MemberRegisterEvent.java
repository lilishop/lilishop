package cn.lili.event;

import cn.lili.modules.member.entity.dos.Member;

/**
 * 会员注册消息
 *
 * @author Chopper
 * @since 2020/11/17 7:13 下午
 */
public interface MemberRegisterEvent {

    /**
     * 会员注册
     *
     * @param member 会员
     */
    void memberRegister(Member member);
}
