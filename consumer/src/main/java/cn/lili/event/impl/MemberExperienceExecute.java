package cn.lili.event.impl;


import cn.lili.event.GoodsCommentCompleteEvent;
import cn.lili.event.MemberRegisterEvent;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberEvaluation;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.ExperienceSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.google.gson.Gson;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 会员经验值
 *
 * @author Bulbasaur
 * @date: 2021/5/16 11:16 下午
 */
@Service
public class MemberExperienceExecute implements MemberRegisterEvent, GoodsCommentCompleteEvent {

    //配置
    @Autowired
    private SettingService settingService;
    //会员
    @Autowired
    private MemberService memberService;

    @Override
    public void memberRegister(Member member) {
        //获取经验值设置
        Setting setting = settingService.get(SettingEnum.EXPERIENCE_SETTING.name());
        ExperienceSetting experienceSetting = new Gson().fromJson(setting.getSettingValue(), ExperienceSetting.class);
        //赠送会员经验值
        memberService.updateMemberPoint(Long.valueOf(experienceSetting.getRegister().longValue()), 1, member.getId(), "会员注册，赠送经验值" + experienceSetting.getRegister());
    }

    @Override
    public void goodsComment(MemberEvaluation memberEvaluation) {
        //获取签到经验值设置
        Setting setting = settingService.get(SettingEnum.EXPERIENCE_SETTING.name());
        ExperienceSetting experienceSetting = new Gson().fromJson(setting.getSettingValue(), ExperienceSetting.class);
        //赠送会员经验值
        memberService.updateMemberPoint(Long.valueOf(experienceSetting.getComment().longValue()), 1, memberEvaluation.getMemberId(), "会员评价，赠送经验值" + experienceSetting.getComment());
    }
}
