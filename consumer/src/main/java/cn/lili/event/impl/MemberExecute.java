package cn.lili.event.impl;

import cn.hutool.core.util.StrUtil;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.event.MemberConnectLoginEvent;
import cn.lili.event.MemberLoginEvent;
import cn.lili.modules.connect.entity.dto.ConnectAuthUser;
import cn.lili.modules.connect.entity.enums.ConnectEnum;
import cn.lili.modules.connect.entity.enums.SourceEnum;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.system.service.SettingService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 会员自身业务
 * 会员登录，会员第三方登录
 *
 * @author Chopper
 * @version v1.0
 * 2022-01-11 11:08
 */
@Slf4j
@Service
public class MemberExecute implements MemberLoginEvent, MemberConnectLoginEvent {
    @Autowired
    private MemberService memberService;
    @Autowired
    private ConnectService connectService;
    @Autowired
    private SettingService settingService;

    @Override
    public void memberLogin(Member member) {
        memberService.updateMemberLoginTime(member.getId());
    }

    @Override
    public void memberConnectLogin(Member member, ConnectAuthUser authUser) {
        //保存UnionID
        if (StrUtil.isNotBlank(authUser.getToken().getUnionId())) {
            connectService.loginBindUser(member.getId(), authUser.getToken().getUnionId(), authUser.getSource().name());
        }
        //保存OpenID
        if (StrUtil.isNotBlank(authUser.getUuid())) {
            SourceEnum sourceEnum = SourceEnum.getSourceEnum(authUser.getSource(), authUser.getType());
            connectService.loginBindUser(member.getId(), authUser.getUuid(), sourceEnum.name());
        }
        //保存手机号，判断用户是否存手机号，如果不存在则保存手机号
        if (StrUtil.isNotBlank(authUser.getPhone())&&StrUtil.isBlank(member.getMobile())) {
            memberService.changeMobile(member.getId(),member.getMobile());
        }

    }
}
