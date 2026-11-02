package cn.lili.modules.wechat.util;

import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.wechat.entity.dos.WechatMPMessage;
import cn.lili.modules.wechat.service.WechatMPMessageService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
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
    private ConnectService connectService;
    private WechatAccessTokenUtil wechatAccessTokenUtil;

    @BeforeEach
    void setUp() {
        sender = spy(new WechatSceneMessageSender());
        wechatMPMessageService = mock(WechatMPMessageService.class);
        connectService = mock(ConnectService.class);
        wechatAccessTokenUtil = mock(WechatAccessTokenUtil.class);
        ReflectionTestUtils.setField(sender, "wechatMPMessageService", wechatMPMessageService);
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
}
