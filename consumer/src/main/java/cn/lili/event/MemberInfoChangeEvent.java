package cn.lili.event;

import cn.lili.modules.member.entity.dos.Member;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
public interface MemberInfoChangeEvent {

    /**
     * 会员信息更改消息
     *
     * @param member 会员信息
     */
    void memberInfoChange(Member member);
}
