package cn.lili.modules.wechat.util;

import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.wechat.entity.dos.WechatMPMessage;
import cn.lili.modules.wechat.entity.dos.WechatMessage;
import cn.lili.modules.wechat.service.WechatMPMessageService;
import cn.lili.modules.wechat.service.WechatMessageService;
import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.slf4j.LoggerFactory;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 场景化微信消息发送（小程序订阅消息）
 */
class WechatSceneMessageSenderTest {

    private WechatSceneMessageSender sender;
    private WechatMPMessageService wechatMPMessageService;
    private WechatMessageService wechatMessageService;
    private ConnectService connectService;
    private WechatAccessTokenUtil wechatAccessTokenUtil;

    @BeforeEach
    void setUp() {
        sender = spy(new WechatSceneMessageSender());
        wechatMPMessageService = mock(WechatMPMessageService.class);
        wechatMessageService = mock(WechatMessageService.class);
        connectService = mock(ConnectService.class);
        wechatAccessTokenUtil = mock(WechatAccessTokenUtil.class);
        ReflectionTestUtils.setField(sender, "wechatMPMessageService", wechatMPMessageService);
        ReflectionTestUtils.setField(sender, "wechatMessageService", wechatMessageService);
        ReflectionTestUtils.setField(sender, "connectService", connectService);
        ReflectionTestUtils.setField(sender, "wechatAccessTokenUtil", wechatAccessTokenUtil);
        doReturn("{\"errcode\":0,\"errmsg\":\"ok\"}").when(sender).postJson(anyString(), anyString());
    }

    private WechatMPMessage enabledTemplate() {
        WechatMPMessage template = new WechatMPMessage();
        template.setSceneCode("ORDER_DELIVER");
        template.setCode("MP_TEMPLATE_ID");
        template.setEnable(true);
        template.setKeywords("[\"ORDER_SN\",\"GOODS_INFO\"]");
        template.setKeywordsText("[\"订单编号\",\"商品名称\"]");
        return template;
    }

    private Connect mpConnect() {
        Connect connect = new Connect();
        connect.setUnionId("mp_open_id_1");
        return connect;
    }

    @Test
    void skipsWhenTemplateMissingOrDisabled() {
        when(wechatMPMessageService.getOne(any(LambdaQueryWrapper.class), any(Boolean.class))).thenReturn(null);
        sender.sendMpMessage("ORDER_DELIVER", "m1", Map.of("order_sn", "SN123"), "pages/order/orderDetail?sn=SN123");

        WechatMPMessage disabled = enabledTemplate();
        disabled.setEnable(false);
        when(wechatMPMessageService.getOne(any(LambdaQueryWrapper.class), any(Boolean.class))).thenReturn(disabled);
        sender.sendMpMessage("ORDER_DELIVER", "m1", Map.of("order_sn", "SN123"), "pages/order/orderDetail?sn=SN123");

        WechatMPMessage noCode = enabledTemplate();
        noCode.setCode(null);
        when(wechatMPMessageService.getOne(any(LambdaQueryWrapper.class), any(Boolean.class))).thenReturn(noCode);
        sender.sendMpMessage("ORDER_DELIVER", "m1", Map.of("order_sn", "SN123"), "pages/order/orderDetail?sn=SN123");

        verify(sender, never()).postJson(anyString(), anyString());
    }

    @Test
    void skipsWhenConnectMissing() {
        when(wechatMPMessageService.getOne(any(LambdaQueryWrapper.class), any(Boolean.class))).thenReturn(enabledTemplate());
        when(connectService.queryConnect(any())).thenReturn(null);
        sender.sendMpMessage("ORDER_DELIVER", "m1", Map.of("order_sn", "SN123"), "pages/order/orderDetail?sn=SN123");
        verify(sender, never()).postJson(anyString(), anyString());
    }

    @Test
    void sendsWithRenderedKeywords() {
        when(wechatMPMessageService.getOne(any(LambdaQueryWrapper.class), any(Boolean.class))).thenReturn(enabledTemplate());
        when(connectService.queryConnect(any())).thenReturn(mpConnect());
        when(wechatAccessTokenUtil.cgiAccessToken(any())).thenReturn("token");

        Map<String, String> params = new HashMap<>();
        params.put("order_sn", "SN123");
        params.put("goods_info", "手机");
        sender.sendMpMessage("ORDER_DELIVER", "m1", params, "pages/order/orderDetail?sn=SN123");

        ArgumentCaptor<String> bodyCaptor = ArgumentCaptor.forClass(String.class);
        verify(sender).postJson(anyString(), bodyCaptor.capture());
        String body = bodyCaptor.getValue();
        assertThat(body).contains("SN123").contains("手机").contains("MP_TEMPLATE_ID")
                .contains("mp_open_id_1").contains("pages/order/orderDetail?sn=SN123")
                .contains("订单编号").contains("商品名称");
    }

    @Test
    void rendersMissingKeywordAsDash() {
        when(wechatMPMessageService.getOne(any(LambdaQueryWrapper.class), any(Boolean.class))).thenReturn(enabledTemplate());
        when(connectService.queryConnect(any())).thenReturn(mpConnect());
        when(wechatAccessTokenUtil.cgiAccessToken(any())).thenReturn("token");

        Map<String, String> params = new HashMap<>();
        params.put("order_sn", "SN123");
        sender.sendMpMessage("ORDER_DELIVER", "m1", params, "pages/order/orderDetail?sn=SN123");

        ArgumentCaptor<String> bodyCaptor = ArgumentCaptor.forClass(String.class);
        verify(sender).postJson(anyString(), bodyCaptor.capture());
        assertThat(bodyCaptor.getValue()).contains("商品名称\":{\"value\":\"-\"}");
    }

    private WechatMessage enabledOaTemplate() {
        WechatMessage template = new WechatMessage();
        template.setSceneCode("ORDER_DELIVER");
        template.setCode("OA_TEMPLATE_ID");
        template.setEnable(true);
        template.setFirst("您好");
        template.setRemark("谢谢");
        template.setKeywords("ORDER_SN,MEMBER_NAME");
        return template;
    }

    private Connect oaConnect() {
        Connect connect = new Connect();
        connect.setUnionId("oa_open_id_1");
        return connect;
    }

    @Test
    void oaSkipsWhenDisabledOrCodeBlank() {
        WechatMessage disabled = enabledOaTemplate();
        disabled.setEnable(false);
        when(wechatMessageService.getOne(any(LambdaQueryWrapper.class), any(Boolean.class))).thenReturn(disabled);
        sender.sendOaMessage("ORDER_DELIVER", "m1", Map.of("order_sn", "SN123"));

        WechatMessage noCode = enabledOaTemplate();
        noCode.setCode(null);
        when(wechatMessageService.getOne(any(LambdaQueryWrapper.class), any(Boolean.class))).thenReturn(noCode);
        sender.sendOaMessage("ORDER_DELIVER", "m1", Map.of("order_sn", "SN123"));

        verify(sender, never()).postJson(anyString(), anyString());
    }

    @Test
    void oaSendsFirstKeywordRemark() {
        when(wechatMessageService.getOne(any(LambdaQueryWrapper.class), any(Boolean.class))).thenReturn(enabledOaTemplate());
        when(connectService.queryConnect(any())).thenReturn(oaConnect());
        when(wechatAccessTokenUtil.cgiAccessToken(any())).thenReturn("token");

        Map<String, String> params = new HashMap<>();
        params.put("order_sn", "SN123");
        params.put("member_name", "张三");
        sender.sendOaMessage("ORDER_DELIVER", "m1", params);

        ArgumentCaptor<String> bodyCaptor = ArgumentCaptor.forClass(String.class);
        verify(sender).postJson(anyString(), bodyCaptor.capture());
        String body = bodyCaptor.getValue();
        assertThat(body).contains("OA_TEMPLATE_ID").contains("oa_open_id_1")
                .contains("first\":{\"value\":\"您好\"}")
                .contains("keyword1\":{\"value\":\"SN123\"}")
                .contains("keyword2\":{\"value\":\"张三\"}")
                .contains("remark\":{\"value\":\"谢谢\"}");
    }

    @Test
    void oaRendersMissingKeywordAsDash() {
        when(wechatMessageService.getOne(any(LambdaQueryWrapper.class), any(Boolean.class))).thenReturn(enabledOaTemplate());
        when(connectService.queryConnect(any())).thenReturn(oaConnect());
        when(wechatAccessTokenUtil.cgiAccessToken(any())).thenReturn("token");

        sender.sendOaMessage("ORDER_DELIVER", "m1", Map.of("order_sn", "SN123"));

        ArgumentCaptor<String> bodyCaptor = ArgumentCaptor.forClass(String.class);
        verify(sender).postJson(anyString(), bodyCaptor.capture());
        assertThat(bodyCaptor.getValue()).contains("keyword2\":{\"value\":\"-\"}");
    }

    @Test
    void checkResultLogsErrorWhenErrcodeNonZero() {
        Logger logger = (Logger) LoggerFactory.getLogger(WechatSceneMessageSender.class);
        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.start();
        logger.addAppender(appender);
        try {
            sender.checkResult("微信公众号模板消息", "ORDER_DELIVER", "m1", "{\"errcode\":43004,\"errmsg\":\"require subscribe\"}");
        } finally {
            logger.detachAppender(appender);
        }
        assertThat(appender.list).anySatisfy(event -> {
            assertThat(event.getLevel()).isEqualTo(Level.ERROR);
            assertThat(event.getFormattedMessage()).contains("43004");
        });
    }

    @Test
    void checkResultQuietWhenErrcodeZeroOrUnparsable() {
        Logger logger = (Logger) LoggerFactory.getLogger(WechatSceneMessageSender.class);
        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.start();
        logger.addAppender(appender);
        try {
            sender.checkResult("小程序订阅消息", "ORDER_DELIVER", "m1", "{\"errcode\":0,\"errmsg\":\"ok\"}");
            sender.checkResult("小程序订阅消息", "ORDER_DELIVER", "m1", "not-json");
        } finally {
            logger.detachAppender(appender);
        }
        assertThat(appender.list).noneMatch(event -> event.getLevel() == Level.ERROR);
    }
}
