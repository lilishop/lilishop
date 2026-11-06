package cn.lili.listener;

import cn.lili.message.MemberMessageOrchestrator;
import cn.lili.message.QueueMessage;
import cn.lili.modules.message.entity.dto.NoticeMessageDTO;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * 站内信通知消费者容错测试：版本错配时未知消息节点应跳过并告警，
 * 已知节点正常反序列化并交给消息编排器分发
 *
 * @author Bulbasaur
 * @since 2026-11-06
 */
class NoticeMessageListenerTest {

    private final NoticeMessageListener listener = new NoticeMessageListener();
    private final MemberMessageOrchestrator orchestrator = mock(MemberMessageOrchestrator.class);

    /**
     * 初始化消息编排器的模拟依赖。
     */
    NoticeMessageListenerTest() {
        ReflectionTestUtils.setField(listener, "memberMessageOrchestrator", orchestrator);
    }

    /**
     * 构造指定节点名称的站内信通知消息。
     *
     * @param nodeName 消息节点枚举名
     * @return 队列消息
     */
    private QueueMessage buildMessage(String nodeName) {
        String body = "{\"memberId\":\"m1\",\"noticeMessageNodeEnum\":\"" + nodeName + "\",\"parameter\":{\"income\":\"1.00\"}}";
        return new QueueMessage("lili_notice_topic", null, body.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * 未知节点（新版发送方特有）不进入编排器，也不抛异常进入重试/死信。
     */
    @Test
    void unknownNodeIsSkippedWithoutException() {
        assertDoesNotThrow(() -> listener.onMessage(buildMessage("FUTURE_NODE_NOT_DEPLOYED")));
        verifyNoInteractions(orchestrator);
    }

    /**
     * 已知节点正常反序列化并分发给消息编排器。
     */
    @Test
    void knownNodeIsDispatched() {
        listener.onMessage(buildMessage("WALLET_CHANGE"));
        verify(orchestrator).orchestrate(any(NoticeMessageDTO.class));
    }
}
