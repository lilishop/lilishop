package cn.lili.dispatcher.member;

import cn.hutool.json.JSONUtil;
import cn.lili.dispatcher.MemberMqDispatcher;
import cn.lili.event.MemberPointChangeEvent;
import cn.lili.modules.member.entity.dto.MemberPointMessage;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

import static cn.lili.rocketmq.tags.MemberTagsEnum.MEMBER_POINT_CHANGE;
import static cn.lili.rocketmq.tags.MemberTagsEnum.MEMBER_REGISTER;

@Slf4j
@RequiredArgsConstructor
@Component
public class MemberPointChangeDispatcher implements MemberMqDispatcher {

    /**
     * 会员积分变化
     */
    @Autowired
    private final List<MemberPointChangeEvent> memberPointChangeEvents;

    @Override
    public String supportTag() {
        return MEMBER_POINT_CHANGE.name();
    }

    @Override
    public void dispatch(MessageExt messageExt) {
        MemberPointMessage memberPointMessage = JSONUtil.toBean(new String(messageExt.getBody()), MemberPointMessage.class);
        for (MemberPointChangeEvent memberPointChangeEvent : memberPointChangeEvents) {
            try {
                memberPointChangeEvent.memberPointChange(memberPointMessage);
            } catch (Exception e) {
                log.error("执行会员积分变化相关分发逻辑出错 {}", e.getMessage());
            }
        }
    }
}
