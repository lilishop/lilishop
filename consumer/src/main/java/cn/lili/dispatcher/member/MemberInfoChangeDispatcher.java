package cn.lili.dispatcher.member;

import cn.hutool.json.JSONUtil;
import cn.lili.dispatcher.MemberMqDispatcher;
import cn.lili.event.MemberInfoChangeEvent;
import cn.lili.modules.member.entity.dos.Member;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.springframework.stereotype.Component;

import java.util.List;

import static cn.lili.rocketmq.tags.MemberTagsEnum.MEMBER_INFO_EDIT;
import static cn.lili.rocketmq.tags.MemberTagsEnum.MEMBER_REGISTER;

@Slf4j
@RequiredArgsConstructor
@Component
public class MemberInfoChangeDispatcher implements MemberMqDispatcher {

    /**
     * 会员信息变更
     */
    private final List<MemberInfoChangeEvent> memberInfoChangeEvents;

    @Override
    public String supportTag() {
        return MEMBER_INFO_EDIT.name();
    }

    @Override
    public void dispatch(MessageExt messageExt) {
        Member member = JSONUtil.toBean(new String(messageExt.getBody()), Member.class);
        for (MemberInfoChangeEvent memberInfoChangeEvent : memberInfoChangeEvents) {
            try {
                memberInfoChangeEvent.memberInfoChange(member);
            } catch (Exception e) {
                log.error("执行会员信息变更相关分发逻辑出错 {}", e.getMessage());
            }
        }
    }
}
