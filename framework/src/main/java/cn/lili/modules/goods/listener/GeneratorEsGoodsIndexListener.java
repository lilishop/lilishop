package cn.lili.modules.goods.listener;

import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.modules.goods.event.GeneratorEsGoodsIndexEvent;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
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
        String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.GENERATOR_GOODS_INDEX.name();
        //发送mq消息
        rocketMQTemplate.asyncSend(destination, esGoodsIndexEvent.getGoodsId(), RocketmqSendCallbackBuilder.commonCallback());
    }

}
