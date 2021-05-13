package cn.lili.scocket.listener;

import cn.hutool.json.JSONUtil;
import cn.lili.common.rocketmq.tags.OtherTagsEnum;
import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.message.entity.dos.Message;
import cn.lili.modules.message.entity.enums.MessageShowType;
import cn.lili.modules.message.entity.vos.MessageShowVO;
import cn.lili.modules.message.mapper.StoreMessageMapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.CrossOrigin;

/**
 * @author paulG
 * @since 2020/12/9
 **/
@Component
@CrossOrigin
@Slf4j
@RocketMQMessageListener(topic = "${lili.data.rocketmq.other-topic}", consumerGroup = "${lili.data.rocketmq.other-group}")
public class MessageSendListener implements RocketMQListener<MessageExt> {



    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @Override
    public void onMessage(MessageExt messageExt) {
        log.info(messageExt.getTags());
        switch (OtherTagsEnum.valueOf(messageExt.getTags())) {
            //站内消息提醒
            case MESSAGE:
                System.out.println("消息提醒");
                sendNoticeMessage(messageExt);
                break;
            default:
                break;
        }
    }

    /**
     * 给商家发送站内信息
     *
     * @param messageExt
     */
    private void sendNoticeMessage(MessageExt messageExt) {
        MessageShowVO messageVO = new MessageShowVO();
        Message message = JSONUtil.toBean(new String(messageExt.getBody()), Message.class);
        //构建vo
        BeanUtil.copyProperties(message, messageVO);
        messageVO.setType(MessageShowType.NOTICE.name());
        if (message.getMessageRange().equals("ALL")) {
            simpMessagingTemplate.convertAndSend("/topic/subscribe", messageVO);
        } else {
            for (String id : message.getUserIds()) {
                simpMessagingTemplate.convertAndSendToUser("SHOP_" + id, "/queue/subscribe", messageVO);
            }
        }
    }
}
