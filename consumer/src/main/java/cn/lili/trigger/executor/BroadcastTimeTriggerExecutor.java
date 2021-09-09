package cn.lili.trigger.executor;

import cn.hutool.json.JSONUtil;
import cn.lili.modules.goods.service.StudioService;
import cn.lili.trigger.TimeTriggerExecutor;
import cn.lili.trigger.message.BroadcastMessage;
import cn.lili.trigger.model.TimeExecuteConstant;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 直播间事件触发
 *
 * @author Bulbasaur
 * @since 2021/6/1 5:02 下午
 */
@Slf4j
@Component(TimeExecuteConstant.BROADCAST_EXECUTOR)
public class BroadcastTimeTriggerExecutor implements TimeTriggerExecutor {


    @Autowired
    private StudioService studioService;

    @Override
    public void execute(Object object) {
        //直播间订单消息
        BroadcastMessage broadcastMessage = JSONUtil.toBean(JSONUtil.parseObj(object), BroadcastMessage.class);
        if (broadcastMessage != null && broadcastMessage.getStudioId() != null) {
            log.info("直播间消费：{}", broadcastMessage);
            //修改直播间状态
            studioService.updateStudioStatus(broadcastMessage);
        }
    }
}
