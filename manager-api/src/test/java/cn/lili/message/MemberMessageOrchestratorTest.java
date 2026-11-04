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
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.ArgumentCaptor;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
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

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
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

    /**
     * 构造所有通知渠道均开启的会员通知模板。
     *
     * @return 所有通知渠道均开启的会员通知模板
     */
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

    /**
     * 构造会员发货通知参数。
     *
     * @return 会员发货通知参数
     */
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

    /**
     * 模拟指定签名及测试模式的短信配置。
     *
     * @param signName  短信签名
     * @param testModel 短信测试模式
     */
    private void mockSmsSetting(String signName, boolean testModel) {
        Setting setting = new Setting();
        setting.setSettingValue("{\"signName\":" + (signName == null ? "null" : "\"" + signName + "\"") + ",\"isTestModel\":" + testModel + "}");
        when(settingService.get(SettingEnum.SMS_SETTING.name())).thenReturn(setting);
    }

    /**
     * 模拟已填写手机号的会员查询结果。
     */
    private void mockMemberWithMobile() {
        Member member = new Member();
        member.setMobile("13800000000");
        when(memberService.getById("m1")).thenReturn(member);
    }

    /**
     * 验证通知模板不存在时直接返回。
     */
    @Test
    void returnsQuietlyWhenTemplateMissing() {
        when(noticeMessageService.getOne(any(LambdaQueryWrapper.class), anyBoolean())).thenReturn(null);
        orchestrator.orchestrate(dto());
        verifyNoInteractions(smsUtil, wechatSceneMessageSender, memberService, settingService);
        verify(noticeMessageService, never()).noticeMessage(any());
    }

    /**
     * 验证所有通知渠道开启时分别执行发送。
     */
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

    /**
     * 验证会员手机号为空时跳过短信发送。
     */
    @Test
    void skipsSmsWhenMemberMobileBlank() {
        when(noticeMessageService.getOne(any(LambdaQueryWrapper.class), anyBoolean())).thenReturn(openTemplate());
        when(memberService.getById("m1")).thenReturn(new Member());

        orchestrator.orchestrate(dto());

        verify(smsUtil, never()).sendNoticeSms(anyString(), anyString(), anyString(), any());
        verify(noticeMessageService).noticeMessage(any(NoticeMessageDTO.class));
    }

    /**
     * 验证缺少短信签名时跳过短信发送。
     */
    @Test
    void skipsSmsWhenSignMissing() {
        when(noticeMessageService.getOne(any(LambdaQueryWrapper.class), anyBoolean())).thenReturn(openTemplate());
        mockMemberWithMobile();
        mockSmsSetting(null, false);

        orchestrator.orchestrate(dto());

        verify(smsUtil, never()).sendNoticeSms(anyString(), anyString(), anyString(), any());
        verify(noticeMessageService).noticeMessage(any(NoticeMessageDTO.class));
    }

    /**
     * 验证测试模式下跳过真实短信发送。
     */
    @Test
    void skipsSmsInTestModel() {
        when(noticeMessageService.getOne(any(LambdaQueryWrapper.class), anyBoolean())).thenReturn(openTemplate());
        mockMemberWithMobile();
        mockSmsSetting("商城平台", true);

        orchestrator.orchestrate(dto());

        verify(smsUtil, never()).sendNoticeSms(anyString(), anyString(), anyString(), any());
    }

    /**
     * 验证短信开关关闭时跳过发送。
     */
    @Test
    void skipsSmsWhenSwitchOff() {
        NoticeMessage template = openTemplate();
        template.setSmsStatus("CLOSE");
        when(noticeMessageService.getOne(any(LambdaQueryWrapper.class), anyBoolean())).thenReturn(template);

        orchestrator.orchestrate(dto());

        verifyNoInteractions(smsUtil, memberService, settingService);
        verify(noticeMessageService).noticeMessage(any(NoticeMessageDTO.class));
    }

    /**
     * 验证单个通知渠道异常不影响其他渠道。
     */
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

    /**
     * 验证通知编排异常不会向调用方抛出。
     */
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

    /**
     * 验证通知模板查询限定会员模板类型。
     */
    @Test
    @SuppressWarnings("unchecked")
    void templateLookupFiltersMemberType() {
        // LambdaQueryWrapper 解析列名需要 TableInfo，单测无 MyBatis 上下文，手动初始化
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), ""), NoticeMessage.class);
        when(noticeMessageService.getOne(any(LambdaQueryWrapper.class), anyBoolean())).thenReturn(null);

        orchestrator.orchestrate(dto());

        ArgumentCaptor<LambdaQueryWrapper<NoticeMessage>> captor = ArgumentCaptor.forClass(LambdaQueryWrapper.class);
        verify(noticeMessageService).getOne(captor.capture(), anyBoolean());
        // 必须带 type=MEMBER 过滤，避免命中同 sceneCode/文案的商户或平台模板
        assertThat(captor.getValue().getSqlSegment()).contains("type");
    }
}
