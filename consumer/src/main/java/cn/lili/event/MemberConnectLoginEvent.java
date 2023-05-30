package cn.lili.event;

import cn.lili.modules.connect.entity.dto.ConnectAuthUser;
import cn.lili.modules.member.entity.dos.Member;

/**
 * 会员联合登录消息
 *
 * @author Chopper
 * @since 2020/11/17 7:13 下午
 */
public interface MemberConnectLoginEvent {

    /**
     * 会员联合登录
     *
     * @param member 会员
     * @param authUser 第三方登录
     */
    void memberConnectLogin(Member member, ConnectAuthUser authUser);
}
