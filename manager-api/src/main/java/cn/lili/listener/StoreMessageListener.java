package cn.lili.listener;

import cn.hutool.json.JSONUtil;
import cn.lili.event.MemberRegisterEvent;
import cn.lili.event.StoreSettingChangeEvent;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.message.QueueMessage;
import cn.lili.rocketmq.tags.StoreTagsEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 店铺消息
 * @author chc
 * @since 2022/6/2114:46
 */
@Component
@Slf4j
public class StoreMessageListener {
    @Autowired
    private List<StoreSettingChangeEvent> storeSettingChangeEventList;

    public void onMessage(QueueMessage messageExt) {
        switch (StoreTagsEnum.valueOf(messageExt.getTags())){
            //修改店铺
            case EDIT_STORE_SETTING:
                for (StoreSettingChangeEvent storeSettingChangeEvent : storeSettingChangeEventList) {
                    try {
                        Store store = JSONUtil.toBean(new String(messageExt.getBody()), Store.class);
                        storeSettingChangeEvent.storeSettingChange(store);
                    } catch (Exception e) {
                        log.error("客户{},在{}业务中，状态修改事件执行异常",
                                new String(messageExt.getBody()),
                                storeSettingChangeEvent.getClass().getName(),
                                e);
                    }
                }
                break;
            default:
                break;
        }
    }
}
