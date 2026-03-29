package cn.lili.dispatcher.member;

import cn.hutool.json.JSONUtil;
import cn.lili.dispatcher.MemberMqDispatcher;
import cn.lili.event.MemberLoginEvent;
import cn.lili.modules.member.entity.dos.Member;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.springframework.stereotype.Component;

import java.util.List;

import static cn.lili.rocketmq.tags.MemberTagsEnum.MEMBER_LOGIN;
import static cn.lili.rocketmq.tags.MemberTagsEnum.MEMBER_REGISTER;

@Slf4j
@RequiredArgsConstructor
@Component
public class MemberLoginDispatcher implements MemberMqDispatcher {

    /**
     * 会员登录
     */
    private final List<MemberLoginEvent> memberLoginEvents;

    @Override
    public String supportTag() {
        return MEMBER_LOGIN.name();
    }

    @Override
    public void dispatch(MessageExt messageExt) {
        Member member = JSONUtil.toBean(new String(messageExt.getBody()), Member.class);
        for (MemberLoginEvent memberLoginEvent : memberLoginEvents) {
            try {
                memberLoginEvent.memberLogin(member);
            } catch (Exception e) {
                log.error("执行会员登录相关分发逻辑出错 {}", e.getMessage());
            }
        }
    }
}
