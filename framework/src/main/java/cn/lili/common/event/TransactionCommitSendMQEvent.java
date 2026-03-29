package cn.lili.common.event;

import lombok.Getter;
import org.springframework.context.ApplicationEvent;

/*
 * 事务提交后发生mq事件
 * 神技效果： 当 Spring 的多播器把事件分发给这个监听器时，监听器会挂起（等待）！它会死死盯着当前数据库的事务，
 * 直到数据库确确实实把 COMMIT 指令执行成功了，它才会激活并真正去执行发送 RocketMQ 的代码！
 * 如果数据库回滚了，这个事件直接丢弃，MQ 绝对不会发出去！完美解决了业务落库和 MQ 发送的一致性！
 *
 * 没有复杂的 if-else，没有恶心的分布式锁，仅仅靠一个 @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)，
 * 就完美解决了微服务里最臭名昭著的**“双写一致性”**难题。
 * 绝对的先后顺序： 如果主业务线里的 @Transactional 方法报错了，数据库触发了 ROLLBACK（回滚）。
 * 这个监听器会像死了一样保持沉默，MQ 消息绝对不会发出去。只有当数据库真正执行了 COMMIT，它才会苏醒并开火。
 * 极低的性能损耗： 很多人一听到“保证数据库和 MQ 一致”，第一反应就是上 Seata 这种重量级的分布式事务（2PC/TCC）。
 * 那玩意儿性能极差，会严重拖垮接口 RT（响应时间）。而 Spring 的这个原生注解，走的是内存级别的状态机监听，性能损耗几乎为 0！
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
    private final Object message;

    public TransactionCommitSendMQEvent(Object source, String topic, String tag, Object message) {
        super(source);
        this.topic = topic;
        this.tag = tag;
        this.message = message;
    }
}
