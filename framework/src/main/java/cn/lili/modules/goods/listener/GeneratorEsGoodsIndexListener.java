package cn.lili.modules.goods.listener;

import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.modules.goods.event.GeneratorEsGoodsIndexEvent;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

/**
 * @author paulG
 * @since 2022/1/19
 **/
@Component
public class GeneratorEsGoodsIndexListener {

    /**
     * rocketMq
     */
    @Autowired
    private RocketMQTemplate rocketMQTemplate;
    /**
     * rocketMq配置
     */
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    public void generatorEsGoodsIndex(GeneratorEsGoodsIndexEvent esGoodsIndexEvent) {
        String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + esGoodsIndexEvent.getTag();
        //发送mq消息
        rocketMQTemplate.asyncSend(destination, esGoodsIndexEvent.getId(), RocketmqSendCallbackBuilder.commonCallback());
    }

}
