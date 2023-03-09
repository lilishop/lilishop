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
        log.info("unionid:"+authUser.getToken().getUnionId());
        log.info("openid:"+authUser.getUuid());
        //保存UnionID
        if (StrUtil.isNotBlank(authUser.getToken().getUnionId())) {
            connectService.loginBindUser(member.getId(), authUser.getToken().getUnionId(), authUser.getSource());
        }
        //保存OpenID
        if (StrUtil.isNotBlank(authUser.getUuid())) {
            log.info("authUser.getSource():"+authUser.getSource());
            log.info("authUser.getType():"+authUser.getType());
            SourceEnum sourceEnum = SourceEnum.getSourceEnum(ConnectEnum.valueOf(authUser.getSource()), ClientTypeEnum.valueOf(authUser.getType()));
            connectService.loginBindUser(member.getId(), authUser.getUuid(), sourceEnum.name());
        }

    }
}
