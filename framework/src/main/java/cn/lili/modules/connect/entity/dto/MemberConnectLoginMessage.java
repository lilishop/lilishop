package cn.lili.modules.connect.entity.dto;

import cn.lili.modules.member.entity.dos.Member;
import lombok.Data;

/**
 * 会员联合登录消息
 */
@Data
public class MemberConnectLoginMessage {

    private Member member;
    private ConnectAuthUser connectAuthUser;
}
