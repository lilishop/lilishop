package cn.lili.modules.appointment.support;

import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.message.MessageSendCallback;
import cn.lili.modules.message.entity.dto.NoticeMessageDTO;
import cn.lili.modules.message.entity.enums.NoticeMessageNodeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 预约通知投递参数测试
 */
class AppointmentNoticeSupportTest {

    private AppointmentNoticeSupport support;
    private LiliMessageTemplate liliMessageTemplate;

    @BeforeEach
    void setUp() {
        support = new AppointmentNoticeSupport();
        liliMessageTemplate = mock(LiliMessageTemplate.class);
        RocketmqCustomProperties properties = mock(RocketmqCustomProperties.class);
        when(properties.getNoticeTopic()).thenReturn("notice-topic");
        ReflectionTestUtils.setField(support, "liliMessageTemplate", liliMessageTemplate);
        ReflectionTestUtils.setField(support, "rocketmqCustomProperties", properties);
    }

    @Test
    void notifyMemberDeliversOrderSnUnderSnakeCaseKey() {
        support.notifyMember("m1", NoticeMessageNodeEnum.APPOINTMENT_BOOKING_SUCCESS_MEMBER, "洗车服务", "SN123");

        ArgumentCaptor<NoticeMessageDTO> captor = ArgumentCaptor.forClass(NoticeMessageDTO.class);
        verify(liliMessageTemplate).asyncSend(eq("notice-topic"), captor.capture(), (MessageSendCallback) isNull());
        // 短信/微信模板变量统一使用 snake_case，预约场景必须携带 order_sn，否则渠道渲染为占位符
        assertThat(captor.getValue().getParameter())
                .containsEntry("order_sn", "SN123")
                .containsEntry("orderSn", "SN123")
                .containsEntry("goods", "洗车服务");
    }
}
