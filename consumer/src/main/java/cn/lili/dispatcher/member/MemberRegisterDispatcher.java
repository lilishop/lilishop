package cn.lili.dispatcher.member;

import cn.hutool.json.JSONUtil;
import cn.lili.dispatcher.MemberMqDispatcher;
import cn.lili.event.MemberRegisterEvent;
import cn.lili.modules.member.entity.dos.Member;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.springframework.stereotype.Component;

import java.util.List;

import static cn.lili.rocketmq.tags.MemberTagsEnum.MEMBER_REGISTER;

@Slf4j
@RequiredArgsConstructor
@Component
public class MemberRegisterDispatcher implements MemberMqDispatcher {

    /**
     * 会员注册
     */
    private final List<MemberRegisterEvent> memberSignEvents;

    @Override
    public String supportTag() {
        return MEMBER_REGISTER.name();
    }

    @Override
    public void dispatch(MessageExt messageExt) {
        Member member = JSONUtil.toBean(new String(messageExt.getBody()), Member.class);
        for (MemberRegisterEvent memberSignEvent : memberSignEvents) {
            try {
                memberSignEvent.memberRegister(member);
            } catch (Exception e) {
                log.error("执行会员注册相关分发逻辑出错 {}", e.getMessage());
            }
        }
    }
}
