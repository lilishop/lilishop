package cn.lili.event;

import cn.lili.modules.member.entity.dos.Member;

/**
 * 客户注册消息
 *
 * @author Chopper
 * @since 2020/11/17 7:13 下午
 */
public interface MemberRegisterEvent {

    /**
     * 客户注册
     *
     * @param member 客户
     */
    void memberRegister(Member member);
}
