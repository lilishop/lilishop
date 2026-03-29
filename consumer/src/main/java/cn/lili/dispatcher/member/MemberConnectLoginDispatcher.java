package cn.lili.dispatcher.member;

import cn.hutool.json.JSONUtil;
import cn.lili.dispatcher.MemberMqDispatcher;
import cn.lili.event.MemberConnectLoginEvent;
import cn.lili.modules.connect.entity.dto.MemberConnectLoginMessage;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.springframework.stereotype.Component;

import java.util.List;

import static cn.lili.rocketmq.tags.MemberTagsEnum.MEMBER_CONNECT_LOGIN;
import static cn.lili.rocketmq.tags.MemberTagsEnum.MEMBER_REGISTER;

@Slf4j
@RequiredArgsConstructor
@Component
public class MemberConnectLoginDispatcher implements MemberMqDispatcher {

    /**
     * 第三方登录
     */
    private final List<MemberConnectLoginEvent> memberConnectLoginEvents;

    @Override
    public String supportTag() {
        return MEMBER_CONNECT_LOGIN.name();
    }

    @Override
    public void dispatch(MessageExt messageExt) {
        MemberConnectLoginMessage connectLoginMessage = JSONUtil.toBean(new String(messageExt.getBody()), MemberConnectLoginMessage.class);
        for (MemberConnectLoginEvent memberConnectLoginEvent : memberConnectLoginEvents) {
            try {
                memberConnectLoginEvent.memberConnectLogin(connectLoginMessage.getMember(),connectLoginMessage.getConnectAuthUser());
            } catch (Exception e) {
                log.error("执行第三方登录相关分发逻辑出错 {}", e.getMessage());
            }
        }
    }
}