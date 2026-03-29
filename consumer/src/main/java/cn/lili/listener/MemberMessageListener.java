package cn.lili.listener;

import cn.lili.dispatcher.MemberMqDispatcher;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 会员消息
 *
 * @author paulG
 * @since 2020/12/9
 **/
@Component
@Slf4j
@RocketMQMessageListener(topic = "${lili.data.rocketmq.member-topic}", consumerGroup = "${lili.data.rocketmq.member-group}")
public class MemberMessageListener implements RocketMQListener<MessageExt> {

    @Autowired
    private List<MemberMqDispatcher> dispatchers;
    // 2. 本地路由表
    Map<String, MemberMqDispatcher> dispatcherMap;

    @Override
    public void onMessage(MessageExt messageExt) {
        dispatcherMap.get(messageExt.getTags()).dispatch(messageExt);
    }

    @PostConstruct
    public void init() {
        dispatcherMap = dispatchers.stream().collect(Collectors.toMap(MemberMqDispatcher::supportTag, e -> e));
    }
}
