package cn.lili.trigger.listen;

import cn.hutool.json.JSONUtil;
import cn.lili.common.trigger.enums.DelayQueueEnums;
import cn.lili.common.trigger.interfaces.TimeTrigger;
import cn.lili.common.trigger.model.TimeTriggerMsg;
import cn.lili.trigger.AbstractDelayQueueListen;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * PromotionTimeTriggerListen
 *
 * @author Chopper
 * @version v1.0
 * 2021-06-11 10:47
 */
@Component
public class PromotionDelayQueueListen extends AbstractDelayQueueListen {

    @Autowired
    private TimeTrigger timeTrigger;

    @Override
    public void invoke(String jobId) {
        timeTrigger.execute(JSONUtil.toBean(jobId, TimeTriggerMsg.class));
    }


    @Override
    public String setDelayQueueName() {
        return DelayQueueEnums.PROMOTION.name();
    }
}
