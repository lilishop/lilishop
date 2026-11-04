package cn.lili.modules.wallet;

import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.modules.message.entity.dto.NoticeMessageDTO;
import cn.lili.modules.message.entity.enums.NoticeMessageNodeEnum;
import cn.lili.modules.wallet.serviceimpl.MemberWalletServiceImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 余额变动通知改为队列异步投递
 */
class MemberWalletNotifyTest {

    private MemberWalletServiceImpl service;
    private LiliMessageTemplate liliMessageTemplate;

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        service = spy(new MemberWalletServiceImpl());
        liliMessageTemplate = mock(LiliMessageTemplate.class);
        RocketmqCustomProperties properties = mock(RocketmqCustomProperties.class);
        ReflectionTestUtils.setField(service, "liliMessageTemplate", liliMessageTemplate);
        ReflectionTestUtils.setField(service, "rocketmqCustomProperties", properties);
        when(properties.getNoticeTopic()).thenReturn("notice-topic");
        doReturn("180.00").when(service).currentBalanceStr("m1");
    }

    /**
     * 验证钱包变动向通知主题发布消息。
     */
    @Test
    void walletChangePublishesToNoticeTopic() {
        ReflectionTestUtils.invokeMethod(service, "notifyWalletChange", "m1", 100.0, false);

        ArgumentCaptor<Object> payloadCaptor = ArgumentCaptor.forClass(Object.class);
        verify(liliMessageTemplate).asyncSend(eq("notice-topic"), payloadCaptor.capture(), any());
        NoticeMessageDTO dto = (NoticeMessageDTO) payloadCaptor.getValue();
        assertThat(dto.getNoticeMessageNodeEnum()).isEqualTo(NoticeMessageNodeEnum.WALLET_CHANGE);
        assertThat(dto.getMemberId()).isEqualTo("m1");
        assertThat(dto.getParameter()).containsEntry("income", "100.00")
                .containsEntry("expenditure", "0.00")
                .containsEntry("change_amount", "+100.00")
                .containsEntry("wallet_balance", "180.00");
        assertThat(dto.getPage()).isEqualTo("pages/mine/deposit/index");
    }

    /**
     * 验证钱包通知发布失败时不影响钱包变动流程。
     */
    @Test
    void walletChangeDoesNotThrowWhenPublishFails() {
        doThrow(new RuntimeException("redis down")).when(liliMessageTemplate)
                .asyncSend(any(), any(), any());

        assertThatCode(() -> ReflectionTestUtils.invokeMethod(service, "notifyWalletChange", "m1", 100.0, true))
                .doesNotThrowAnyException();
    }
}
