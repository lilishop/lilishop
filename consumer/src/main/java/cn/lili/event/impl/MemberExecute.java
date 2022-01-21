package cn.lili.event.impl;

import cn.lili.event.MemberLoginEvent;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 会员自身业务
 *
 * @author Chopper
 * @version v1.0
 * 2022-01-11 11:08
 */
@Service
public class MemberExecute implements MemberLoginEvent {
    @Autowired
    private MemberService memberService;

    @Override
    public void memberLogin(Member member) {
        memberService.updateMemberLoginTime(member.getId());
    }
}
