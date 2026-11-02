package cn.lili.message;

import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.message.entity.dos.NoticeMessage;
import cn.lili.modules.message.entity.dto.NoticeMessageDTO;
import cn.lili.modules.message.entity.enums.NoticeMessageNodeEnum;
import cn.lili.modules.message.service.NoticeMessageService;
import cn.lili.modules.sms.SmsUtil;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wechat.util.WechatSceneMessageSender;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThatCode;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * 会员消息四渠道编排器
 */
class MemberMessageOrchestratorTest {

    private MemberMessageOrchestrator orchestrator;
    private NoticeMessageService noticeMessageService;
    private SmsUtil smsUtil;
    private WechatSceneMessageSender wechatSceneMessageSender;
    private MemberService memberService;
    private SettingService settingService;

    @BeforeEach
    void setUp() {
        orchestrator = new MemberMessageOrchestrator();
        noticeMessageService = mock(NoticeMessageService.class);
        smsUtil = mock(SmsUtil.class);
        wechatSceneMessageSender = mock(WechatSceneMessageSender.class);
        memberService = mock(MemberService.class);
        settingService = mock(SettingService.class);
        ReflectionTestUtils.setField(orchestrator, "noticeMessageService", noticeMessageService);
        ReflectionTestUtils.setField(orchestrator, "smsUtil", smsUtil);
        ReflectionTestUtils.setField(orchestrator, "wechatSceneMessageSender", wechatSceneMessageSender);
        ReflectionTestUtils.setField(orchestrator, "memberService", memberService);
        ReflectionTestUtils.setField(orchestrator, "settingService", settingService);
    }

    private NoticeMessage openTemplate() {
        NoticeMessage template = new NoticeMessage();
        template.setNoticeNode("订单发货通知");
        template.setNoticeTitle("订单发货通知");
        template.setNoticeContent("您的订单 #{order_sn} 已发货");
        template.setNoticeStatus("OPEN");
        template.setSceneCode("ORDER_DELIVER");
        template.setType("MEMBER");
        template.setVariable("order_sn");
        template.setSmsStatus("OPEN");
        template.setSmsTemplateCode("SMS_001");
        return template;
    }

    private NoticeMessageDTO dto() {
        NoticeMessageDTO dto = new NoticeMessageDTO();
        dto.setMemberId("m1");
        dto.setNoticeMessageNodeEnum(NoticeMessageNodeEnum.ORDER_DELIVER);
        Map<String, String> params = new HashMap<>();
        params.put("order_sn", "SN123");
        dto.setParameter(params);
        dto.setPage("pages/order/orderDetail?sn=SN123");
        return dto;
    }

    private void mockSmsSetting(String signName, boolean testModel) {
        Setting setting = new Setting();
        setting.setSettingValue("{\"signName\":" + (signName == null ? "null" : "\"" + signName + "\"") + ",\"isTestModel\":" + testModel + "}");
        when(settingService.get(SettingEnum.SMS_SETTING.name())).thenReturn(setting);
    }

    private void mockMemberWithMobile() {
        Member member = new Member();
        member.setMobile("13800000000");
        when(memberService.getById("m1")).thenReturn(member);
    }

    @Test
    void returnsQuietlyWhenTemplateMissing() {
        when(noticeMessageService.getOne(any(LambdaQueryWrapper.class), anyBoolean())).thenReturn(null);
        orchestrator.orchestrate(dto());
        verifyNoInteractions(smsUtil, wechatSceneMessageSender, memberService, settingService);
        verify(noticeMessageService, never()).noticeMessage(any());
    }

    @Test
    void dispatchesAllChannelsWhenAllOpen() {
        when(noticeMessageService.getOne(any(LambdaQueryWrapper.class), anyBoolean())).thenReturn(openTemplate());
        mockMemberWithMobile();
        mockSmsSetting("商城平台", false);

        orchestrator.orchestrate(dto());

        verify(noticeMessageService).noticeMessage(any(NoticeMessageDTO.class));
        verify(smsUtil).sendNoticeSms(eq("商城平台"), eq("13800000000"), eq("SMS_001"), any());
        verify(wechatSceneMessageSender).sendOaMessage(eq("ORDER_DELIVER"), eq("m1"), any());
        verify(wechatSceneMessageSender).sendMpMessage(eq("ORDER_DELIVER"), eq("m1"), any(), eq("pages/order/orderDetail?sn=SN123"));
    }

    @Test
    void skipsSmsWhenMemberMobileBlank() {
        when(noticeMessageService.getOne(any(LambdaQueryWrapper.class), anyBoolean())).thenReturn(openTemplate());
        when(memberService.getById("m1")).thenReturn(new Member());

        orchestrator.orchestrate(dto());

        verify(smsUtil, never()).sendNoticeSms(anyString(), anyString(), anyString(), any());
        verify(noticeMessageService).noticeMessage(any(NoticeMessageDTO.class));
    }

    @Test
    void skipsSmsWhenSignMissing() {
        when(noticeMessageService.getOne(any(LambdaQueryWrapper.class), anyBoolean())).thenReturn(openTemplate());
        mockMemberWithMobile();
        mockSmsSetting(null, false);

        orchestrator.orchestrate(dto());

        verify(smsUtil, never()).sendNoticeSms(anyString(), anyString(), anyString(), any());
        verify(noticeMessageService).noticeMessage(any(NoticeMessageDTO.class));
    }

    @Test
    void skipsSmsInTestModel() {
        when(noticeMessageService.getOne(any(LambdaQueryWrapper.class), anyBoolean())).thenReturn(openTemplate());
        mockMemberWithMobile();
        mockSmsSetting("商城平台", true);

        orchestrator.orchestrate(dto());

        verify(smsUtil, never()).sendNoticeSms(anyString(), anyString(), anyString(), any());
    }

    @Test
    void skipsSmsWhenSwitchOff() {
        NoticeMessage template = openTemplate();
        template.setSmsStatus("CLOSE");
        when(noticeMessageService.getOne(any(LambdaQueryWrapper.class), anyBoolean())).thenReturn(template);

        orchestrator.orchestrate(dto());

        verifyNoInteractions(smsUtil, memberService, settingService);
        verify(noticeMessageService).noticeMessage(any(NoticeMessageDTO.class));
    }

    @Test
    void channelExceptionDoesNotAffectOthers() {
        when(noticeMessageService.getOne(any(LambdaQueryWrapper.class), anyBoolean())).thenReturn(openTemplate());
        mockMemberWithMobile();
        mockSmsSetting("商城平台", false);
        doThrow(new RuntimeException("db error")).when(noticeMessageService).noticeMessage(any());

        orchestrator.orchestrate(dto());

        verify(smsUtil).sendNoticeSms(anyString(), anyString(), anyString(), any());
        verify(wechatSceneMessageSender).sendOaMessage(anyString(), anyString(), any());
        verify(wechatSceneMessageSender).sendMpMessage(anyString(), anyString(), any(), any());
    }

    @Test
    void orchestrateNeverThrows() {
        when(noticeMessageService.getOne(any(LambdaQueryWrapper.class), anyBoolean())).thenReturn(openTemplate());
        mockMemberWithMobile();
        mockSmsSetting("商城平台", false);
        doThrow(new RuntimeException("x")).when(noticeMessageService).noticeMessage(any());
        doThrow(new RuntimeException("x")).when(smsUtil).sendNoticeSms(anyString(), anyString(), anyString(), any());
        doThrow(new RuntimeException("x")).when(wechatSceneMessageSender).sendOaMessage(anyString(), anyString(), any());
        doThrow(new RuntimeException("x")).when(wechatSceneMessageSender).sendMpMessage(anyString(), anyString(), any(), any());

        assertThatCode(() -> orchestrator.orchestrate(dto())).doesNotThrowAnyException();
    }
}
