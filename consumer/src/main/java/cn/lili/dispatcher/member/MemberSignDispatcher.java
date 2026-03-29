package cn.lili.dispatcher.member;

import cn.hutool.json.JSONUtil;
import cn.lili.dispatcher.MemberMqDispatcher;
import cn.lili.modules.member.entity.dos.MemberSign;
import cn.lili.modules.member.service.MemberSignService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import static cn.lili.rocketmq.tags.MemberTagsEnum.MEMBER_REGISTER;
import static cn.lili.rocketmq.tags.MemberTagsEnum.MEMBER_SING;

@Slf4j
@RequiredArgsConstructor
@Component
public class MemberSignDispatcher implements MemberMqDispatcher {

    /**
     * 会员签到
     */
    @Autowired
    private final MemberSignService memberSignService;

    @Override
    public String supportTag() {
        return MEMBER_SING.name();
    }

    @Override
    public void dispatch(MessageExt messageExt) {
        MemberSign memberSign = JSONUtil.toBean(new String(messageExt.getBody()), MemberSign.class);
        memberSignService.memberSignSendPoint(memberSign.getMemberId(), memberSign.getSignDay());
    }
}
