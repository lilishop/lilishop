package cn.lili.event.impl;


import cn.lili.event.GoodsCommentCompleteEvent;
import cn.lili.event.MemberRegisterEvent;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberEvaluation;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.member.service.MemberWalletService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.PointSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.google.gson.Gson;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 会员积分
 *
 * @author Chopper
 * @date 2020-07-03 11:20
 */
@Service
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class MemberPointExecute implements MemberRegisterEvent, GoodsCommentCompleteEvent {

    //配置
    private final SettingService settingService;
    //会员
    private final MemberService memberService;

    @Override
    public void memberRegister(Member member) {
        //获取签到积分赠送设置
        Setting setting = settingService.get(SettingEnum.POINT_SETTING.name());
        PointSetting pointSetting = new Gson().fromJson(setting.getSettingValue(), PointSetting.class);
        //赠送会员积分
        memberService.updateMemberPoint(Long.valueOf(pointSetting.getRegister().longValue()), 1, member.getId(), "会员注册，赠送积分" + pointSetting.getRegister() + "分");
    }

    @Override
    public void goodsComment(MemberEvaluation memberEvaluation) {
        //获取签到积分赠送设置
        Setting setting = settingService.get(SettingEnum.POINT_SETTING.name());
        PointSetting pointSetting = new Gson().fromJson(setting.getSettingValue(), PointSetting.class);
        //赠送会员积分
        memberService.updateMemberPoint(Long.valueOf(pointSetting.getComment().longValue()), 1, memberEvaluation.getMemberId(), "会员评价，赠送积分" + pointSetting.getComment() + "分");
    }
}
