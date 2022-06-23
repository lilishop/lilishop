package cn.lili.common.event;

import lombok.Getter;
import org.springframework.context.ApplicationEvent;

/**
 * 事务提交后发生mq事件
 *
 * @author paulG
 * @since 2022/1/19
 **/
public class TransactionCommitSendMQEvent extends ApplicationEvent {

    private static final long serialVersionUID = 5885956821347953071L;


    @Getter
    private final String topic;

    @Getter
    private final String tag;

    @Getter
    private final String message;

    public TransactionCommitSendMQEvent(Object source, String topic, String tag, String message) {
        super(source);
        this.topic = topic;
        this.tag = tag;
        this.message = message;
    }
}
