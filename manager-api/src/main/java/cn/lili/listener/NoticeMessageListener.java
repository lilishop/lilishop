package cn.lili.listener;

import cn.hutool.json.JSONUtil;
import cn.lili.message.MemberMessageOrchestrator;
import cn.lili.message.QueueMessage;
import cn.lili.modules.message.entity.dto.NoticeMessageDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 站内信通知
 *
 * @author paulG
 * @since 2020/12/9
 */
@Component
public class NoticeMessageListener {

    /**
     * 会员消息编排器（站内信/短信/微信公众号/小程序四渠道统一分发）
     */
    @Autowired
    private MemberMessageOrchestrator memberMessageOrchestrator;

    public void onMessage(QueueMessage messageExt) {
        NoticeMessageDTO noticeMessageDTO = JSONUtil.toBean(new String(messageExt.getBody()), NoticeMessageDTO.class);
        memberMessageOrchestrator.orchestrate(noticeMessageDTO);
    }
}
