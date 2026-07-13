package cn.lili.event;

import cn.lili.modules.member.entity.dos.Member;

/**
 * 客户登录消息
 *
 * @author Chopper
 * @since 2020/11/17 7:13 下午
 */
public interface MemberLoginEvent {

    /**
     * 客户登录
     *
     * @param member 客户
     */
    void memberLogin(Member member);
}
