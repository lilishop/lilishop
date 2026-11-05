package cn.lili.listener;

import cn.lili.message.QueueMessage;
import cn.lili.modules.message.entity.dos.MemberMessage;
import cn.lili.modules.message.entity.dos.Message;
import cn.lili.modules.message.service.MemberMessageService;
import cn.lili.modules.message.service.StoreMessageService;
import com.alibaba.fastjson2.JSON;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * 验证旧群发任务忽略行为及站内信投递。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
class NoticeSendMessageListenerTest {
    private final NoticeSendMessageListener listener = new NoticeSendMessageListener();
    private final MemberMessageService memberMessages = mock(MemberMessageService.class);
    private final StoreMessageService storeMessages = mock(StoreMessageService.class);

    /** 初始化站内信服务的模拟依赖。 */
    NoticeSendMessageListenerTest() {
        ReflectionTestUtils.setField(listener, "memberMessageService", memberMessages);
        ReflectionTestUtils.setField(listener, "storeMessageService", storeMessages);
    }

    /** 旧任务和未知标签直接忽略，不解析载荷或触发消息投递。 */
    @ParameterizedTest
    @ValueSource(strings = {"SMS", "UNKNOWN"})
    void ignoresRetiredAndUnknownTasksWithoutParsingTheirPayload(String tag) {
        assertDoesNotThrow(() -> listener.onMessage(new QueueMessage("notice-send", tag,
                "invalid legacy payload".getBytes(StandardCharsets.UTF_8))));
        verifyNoInteractions(memberMessages, storeMessages);
    }

    /** 验证指定客户站内信仍写入正确的收件人及正文。 */
    @Test
    void continuesDeliveringSelectedMemberNotices() {
        Message message = new Message();
        message.setId("message-1");
        message.setMessageClient("member");
        message.setMessageRange("SELECT");
        message.setTitle("通知标题");
        message.setContent("通知内容");
        message.setUserIds(new String[]{"member-1"});
        message.setUserNames(new String[]{"客户"});

        listener.onMessage(new QueueMessage("notice-send", "MESSAGE",
                JSON.toJSONBytes(message)));

        ArgumentCaptor<List<MemberMessage>> saved = ArgumentCaptor.forClass(List.class);
        verify(memberMessages).save(saved.capture());
        assertThat(saved.getValue()).hasSize(1);
        assertThat(saved.getValue().get(0).getMemberId()).isEqualTo("member-1");
        assertThat(saved.getValue().get(0).getContent()).isEqualTo("通知内容");
        verifyNoInteractions(storeMessages);
    }
}
