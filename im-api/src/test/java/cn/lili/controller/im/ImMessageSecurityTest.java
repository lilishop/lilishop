package cn.lili.controller.im;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.im.entity.dos.ImMessage;
import cn.lili.modules.im.entity.dos.ImTalk;
import cn.lili.modules.im.entity.dto.MessageQueryParams;
import cn.lili.modules.im.service.ImMessageService;
import cn.lili.modules.im.service.ImTalkService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * 客服IM消息接口安全控制与越权防护测试
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@ExtendWith(MockitoExtension.class)
class ImMessageSecurityTest {

    @Mock
    private ImMessageService imMessageService;

    @Mock
    private ImTalkService imTalkService;

    @InjectMocks
    private ImMessageController imMessageController;

    private MockedStatic<UserContext> userContextMock;

    @BeforeEach
    void setUp() {
        userContextMock = mockStatic(UserContext.class);
    }

    @AfterEach
    void tearDown() {
        if (userContextMock != null) {
            userContextMock.close();
        }
    }

    /**
     * 未登录状态下查看消息详情必须被拦截
     */
    @Test
    @DisplayName("查看消息-未登录拦截")
    void getThrowsWhenNotLogin() {
        userContextMock.when(UserContext::getCurrentUser).thenReturn(null);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imMessageController.get("msg-101"));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_NOT_LOGIN);
        verify(imMessageService, never()).getById(any());
    }

    /**
     * 非消息发送方或接收方查看消息详情必须拒绝访问（防止水平越权）
     */
    @Test
    @DisplayName("查看消息-越权拦截")
    void getThrowsWhenUnauthorized() {
        AuthUser authUser = new AuthUser("attacker", "user-999", "member", "test", UserEnums.MEMBER);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(authUser);

        ImMessage msg = new ImMessage();
        msg.setId("msg-101");
        msg.setFromUser("user-1");
        msg.setToUser("store-1");
        when(imMessageService.getById("msg-101")).thenReturn(msg);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imMessageController.get("msg-101"));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
    }

    /**
     * 消息合法参与方允许查看消息详情
     */
    @Test
    @DisplayName("查看消息-合法参与方放行")
    void getSuccessWhenAuthorized() {
        AuthUser authUser = new AuthUser("receiver", "user-1", "member", "test", UserEnums.MEMBER);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(authUser);

        ImMessage msg = new ImMessage();
        msg.setId("msg-101");
        msg.setFromUser("store-1");
        msg.setToUser("user-1");
        msg.setText("您好，有什么可以帮您？");
        when(imMessageService.getById("msg-101")).thenReturn(msg);

        ResultMessage<ImMessage> result = imMessageController.get("msg-101");
        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getResult().getText()).isEqualTo("您好，有什么可以帮您？");
    }

    /**
     * 未登录状态下查询历史聊天记录必须被拦截
     */
    @Test
    @DisplayName("查询历史记录-未登录拦截")
    void historyMessageThrowsWhenNotLogin() {
        userContextMock.when(UserContext::getCurrentUser).thenReturn(null);

        MessageQueryParams params = new MessageQueryParams();
        params.setTalkId("talk-1");

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imMessageController.historyMessage(params));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_NOT_LOGIN);
        verify(imMessageService, never()).getList(any());
    }

    /**
     * 非会话双方查询聊天记录必须拒绝（防止批量爬取他人聊天记录）
     */
    @Test
    @DisplayName("查询历史记录-越权拦截")
    void historyMessageThrowsWhenUnauthorized() {
        AuthUser authUser = new AuthUser("attacker", "user-999", "member", "test", UserEnums.MEMBER);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(authUser);

        ImTalk talk = new ImTalk();
        talk.setId("talk-1");
        talk.setUserId1("user-1");
        talk.setUserId2("store-1");
        when(imTalkService.getById("talk-1")).thenReturn(talk);

        MessageQueryParams params = new MessageQueryParams();
        params.setTalkId("talk-1");

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imMessageController.historyMessage(params));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(imMessageService, never()).getList(any());
    }

    /**
     * 未登录状态下修改消息必须被拦截
     */
    @Test
    @DisplayName("修改消息-未登录拦截")
    void updateThrowsWhenNotLogin() {
        userContextMock.when(UserContext::getCurrentUser).thenReturn(null);

        ImMessage imMessage = new ImMessage();
        imMessage.setText("篡改后的内容");

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imMessageController.update("msg-1", imMessage));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_NOT_LOGIN);
        verify(imMessageService, never()).updateById(any());
    }

    /**
     * 非原发送人修改消息必须拒绝（防止篡改他人消息）
     */
    @Test
    @DisplayName("修改消息-非发送人篡改拦截")
    void updateThrowsWhenNotSender() {
        AuthUser authUser = new AuthUser("tamperer", "user-999", "member", "test", UserEnums.MEMBER);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(authUser);

        ImMessage existing = new ImMessage();
        existing.setId("msg-1");
        existing.setFromUser("store-1");
        existing.setToUser("user-1");
        when(imMessageService.getById("msg-1")).thenReturn(existing);

        ImMessage updatePayload = new ImMessage();
        updatePayload.setText("恶意篡改文本");

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imMessageController.update("msg-1", updatePayload));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(imMessageService, never()).updateById(any());
    }

    /**
     * 未登录状态下批量删除消息必须被拦截
     */
    @Test
    @DisplayName("删除消息-未登录拦截")
    void delAllByIdsThrowsWhenNotLogin() {
        userContextMock.when(UserContext::getCurrentUser).thenReturn(null);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imMessageController.delAllByIds(Collections.singletonList("msg-1")));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_NOT_LOGIN);
        verify(imMessageService, never()).removeByIds(any());
    }

    /**
     * 删除他人消息时必须拒绝（防止恶意清空证据）
     */
    @Test
    @DisplayName("删除消息-越权拦截")
    void delAllByIdsThrowsWhenNotParticipant() {
        AuthUser authUser = new AuthUser("attacker", "user-999", "member", "test", UserEnums.MEMBER);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(authUser);

        ImMessage msg = new ImMessage();
        msg.setId("msg-1");
        msg.setFromUser("user-1");
        msg.setToUser("store-1");
        when(imMessageService.listByIds(Collections.singletonList("msg-1"))).thenReturn(Collections.singletonList(msg));

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imMessageController.delAllByIds(Collections.singletonList("msg-1")));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(imMessageService, never()).removeByIds(any());
    }
}
