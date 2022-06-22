package cn.lili.common.listener;

import cn.lili.common.event.TransactionCommitSendMQEvent;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

/**
 * 事务提交监听器
 *
 * @author paulG
 * @since 2022/1/19
 **/
@Component
@Slf4j
public class TransactionCommitSendMQListener {

    /**
     * rocketMq
     */
    @Autowired
    private RocketMQTemplate rocketMQTemplate;


    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    public void send(TransactionCommitSendMQEvent event) {
        log.info("事务提交，发送mq信息!{}", event);
        String destination = event.getTopic() + ":" + event.getTag();
        //发送订单变更mq消息
        rocketMQTemplate.asyncSend(destination, event.getMessage(), RocketmqSendCallbackBuilder.commonCallback());
    }


}
