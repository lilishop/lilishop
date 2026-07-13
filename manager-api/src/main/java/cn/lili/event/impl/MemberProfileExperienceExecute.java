package cn.lili.event.impl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.event.MemberInfoChangeEvent;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.enums.ExperienceRuleEnum;
import cn.lili.modules.member.service.MemberExperienceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class MemberProfileExperienceExecute implements MemberInfoChangeEvent {

    @Autowired
    private MemberExperienceService memberExperienceService;

    @Override
    public void memberInfoChange(Member member) {
        if (member == null || CharSequenceUtil.isBlank(member.getId())) {
            return;
        }
        if (isProfileCompleted(member)) {
            memberExperienceService.grantExperience(member.getId(), ExperienceRuleEnum.PROFILE, member.getId(), "完善个人信息，赠送经验值");
        }
    }

    private boolean isProfileCompleted(Member member) {
        if (CharSequenceUtil.isBlank(member.getNickName())) {
            return false;
        }
        if (member.getSex() == null) {
            return false;
        }
        return member.getBirthday() != null;
    }
}
