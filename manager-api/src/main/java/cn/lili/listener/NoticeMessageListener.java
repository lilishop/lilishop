package cn.lili.listener;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.message.MemberMessageOrchestrator;
import cn.lili.message.QueueMessage;
import cn.lili.modules.message.entity.dto.NoticeMessageDTO;
import cn.lili.modules.message.entity.enums.NoticeMessageNodeEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 站内信通知
 *
 * @author paulG
 * @since 2020/12/9
 */
@Slf4j
@Component
public class NoticeMessageListener {

    /**
     * 会员消息编排器（站内信/短信/微信公众号/小程序四渠道统一分发）
     */
    @Autowired
    private MemberMessageOrchestrator memberMessageOrchestrator;

    /**
     * 消费站内信通知消息。
     * 多服务版本错配时，发送方可能携带本服务尚未部署的新消息节点（枚举反序列化会直接失败），
     * 对未知节点跳过并告警，避免通知消息被反复重试后打入死信队列。
     *
     * @param messageExt 队列消息，body 为 NoticeMessageDTO 的 JSON
     */
    public void onMessage(QueueMessage messageExt) {
        String body = new String(messageExt.getBody());
        String nodeName = JSONUtil.parseObj(body).getStr("noticeMessageNodeEnum");
        if (CharSequenceUtil.isNotBlank(nodeName)) {
            try {
                NoticeMessageNodeEnum.valueOf(nodeName);
            } catch (IllegalArgumentException e) {
                log.warn("未知站内信节点[{}]，当前服务版本不支持，消息跳过：{}", nodeName, body);
                return;
            }
        }
        NoticeMessageDTO noticeMessageDTO = JSONUtil.toBean(body, NoticeMessageDTO.class);
        memberMessageOrchestrator.orchestrate(noticeMessageDTO);
    }
}
