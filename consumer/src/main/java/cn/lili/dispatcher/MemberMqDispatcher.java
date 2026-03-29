package cn.lili.dispatcher;

import org.apache.rocketmq.common.message.MessageExt;

public interface MemberMqDispatcher {

    //负责的tag
    String supportTag();

    //执行真正的分发逻辑
    void dispatch(MessageExt messageExt);
}
