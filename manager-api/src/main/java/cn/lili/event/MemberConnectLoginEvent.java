package cn.lili.event;

import cn.lili.modules.connect.entity.dto.ConnectAuthUser;
import cn.lili.modules.member.entity.dos.Member;

/**
 * 客户联合登录消息
 *
 * @author Chopper
 * @since 2020/11/17 7:13 下午
 */
public interface MemberConnectLoginEvent {

    /**
     * 客户联合登录
     *
     * @param member 客户
     * @param authUser 第三方登录
     */
    void memberConnectLogin(Member member, ConnectAuthUser authUser);
}
