package cn.lili.trigger.executor;

import cn.hutool.json.JSONUtil;
import cn.lili.common.trigger.interfaces.TimeTrigger;
import cn.lili.trigger.TimeTriggerExecutor;
import cn.lili.common.trigger.message.PintuanOrderMessage;
import cn.lili.common.trigger.message.PromotionMessage;
import cn.lili.common.trigger.model.TimeExecuteConstant;
import cn.lili.common.trigger.model.TimeTriggerMsg;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.service.PromotionService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 促销事件触发
 *
 * @author Chopper
 * @version v4.1
 * @date 2020/11/17 7:20 下午
 */
@Slf4j
@Component(TimeExecuteConstant.PROMOTION_EXECUTOR)
public class PromotionTimeTriggerExecutor implements TimeTriggerExecutor {
    /**
     * 促销
     */
    @Autowired
    private PromotionService promotionService;
    /**
     * RocketMQ
     */
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    /**
     * 延时任务
     */
    @Autowired
    private TimeTrigger timeTrigger;
    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;


    @Override
    public void execute(Object object) {
        PromotionMessage promotionMessage = JSONUtil.toBean(JSONUtil.parseObj(object), PromotionMessage.class);
        //促销延时信息
        if (promotionMessage != null && promotionMessage.getPromotionId() != null) {
            log.info("促销活动信息消费：{}", promotionMessage);
            //如果为促销活动开始，则需要发布促销活动结束的定时任务
            if (PromotionStatusEnum.START.name().equals(promotionMessage.getPromotionStatus())) {
                if (!promotionService.updatePromotionStatus(promotionMessage)) {
                    log.error("开始促销活动失败: {}", promotionMessage);
                    return;
                }
                //促销活动开始后，设置促销活动结束的定时任务
                promotionMessage.setPromotionStatus(PromotionStatusEnum.END.name());
                String uniqueKey = "{TIME_TRIGGER_" + promotionMessage.getPromotionType() + "}_" + promotionMessage.getPromotionId();
                //结束时间（延时一分钟）
                long closeTime = promotionMessage.getEndTime().getTime() + 60000;
                TimeTriggerMsg timeTriggerMsg = new TimeTriggerMsg(TimeExecuteConstant.PROMOTION_EXECUTOR, closeTime, promotionMessage, uniqueKey, rocketmqCustomProperties.getPromotionTopic());
                //添加延时任务
                timeTrigger.addDelay(timeTriggerMsg);
            } else {
                //不是开始，则修改活动状态
                promotionService.updatePromotionStatus(promotionMessage);
            }
            return;
        }
        //拼团订单消息
        PintuanOrderMessage pintuanOrderMessage = JSONUtil.toBean(JSONUtil.parseObj(object), PintuanOrderMessage.class);
        if (pintuanOrderMessage != null && pintuanOrderMessage.getPintuanId() != null) {
            log.info("拼团订单信息消费：{}", pintuanOrderMessage);
            //拼团订单自动处理
            orderService.agglomeratePintuanOrder(pintuanOrderMessage.getPintuanId(), pintuanOrderMessage.getOrderSn());
        }
    }


}
