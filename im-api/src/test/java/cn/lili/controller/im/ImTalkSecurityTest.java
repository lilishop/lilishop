package cn.lili.controller.im;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.im.entity.dos.ImTalk;
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * 客服IM会话接口安全控制与越权防护测试
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@ExtendWith(MockitoExtension.class)
class ImTalkSecurityTest {

    @Mock
    private ImTalkService imTalkService;

    @InjectMocks
    private ImTalkController imTalkController;

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
     * 未登录状态下查看会话详情必须被拦截
     */
    @Test
    @DisplayName("查看会话-未登录拦截")
    void getThrowsWhenNotLogin() {
        userContextMock.when(UserContext::getCurrentUser).thenReturn(null);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imTalkController.get("talk-101"));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_NOT_LOGIN);
        verify(imTalkService, never()).getById(any());
    }

    /**
     * 非会话参与方查看会话详情必须拒绝访问（防止水平越权）
     */
    @Test
    @DisplayName("查看会话-越权拦截")
    void getThrowsWhenUnauthorized() {
        AuthUser authUser = new AuthUser("attacker", "user-999", "member", "test", UserEnums.MEMBER);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(authUser);

        ImTalk talk = new ImTalk();
        talk.setId("talk-101");
        talk.setUserId1("user-1");
        talk.setUserId2("store-1");
        when(imTalkService.getById("talk-101")).thenReturn(talk);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imTalkController.get("talk-101"));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
    }

    /**
     * 会话合法参与方允许查看会话详情
     */
    @Test
    @DisplayName("查看会话-合法参与方放行")
    void getSuccessWhenAuthorized() {
        AuthUser authUser = new AuthUser("buyer", "user-1", "member", "test", UserEnums.MEMBER);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(authUser);

        ImTalk talk = new ImTalk();
        talk.setId("talk-101");
        talk.setUserId1("user-1");
        talk.setUserId2("store-1");
        when(imTalkService.getById("talk-101")).thenReturn(talk);

        ResultMessage<ImTalk> result = imTalkController.get("talk-101");
        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getResult().getId()).isEqualTo("talk-101");
    }

    /**
     * 未登录状态下设置置顶必须被拦截
     */
    @Test
    @DisplayName("置顶会话-未登录拦截")
    void topThrowsWhenNotLogin() {
        userContextMock.when(UserContext::getCurrentUser).thenReturn(null);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imTalkController.top("talk-101", true));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_NOT_LOGIN);
        verify(imTalkService, never()).top(any(), any());
    }

    /**
     * 非会话参与方设置置顶必须拒绝
     */
    @Test
    @DisplayName("置顶会话-越权拦截")
    void topThrowsWhenUnauthorized() {
        AuthUser authUser = new AuthUser("attacker", "user-999", "member", "test", UserEnums.MEMBER);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(authUser);

        ImTalk talk = new ImTalk();
        talk.setId("talk-101");
        talk.setUserId1("user-1");
        talk.setUserId2("store-1");
        when(imTalkService.getById("talk-101")).thenReturn(talk);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imTalkController.top("talk-101", true));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(imTalkService, never()).top(any(), any());
    }

    /**
     * 未登录状态下删除会话必须被拦截
     */
    @Test
    @DisplayName("删除会话-未登录拦截")
    void disableThrowsWhenNotLogin() {
        userContextMock.when(UserContext::getCurrentUser).thenReturn(null);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imTalkController.disable("talk-101"));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_NOT_LOGIN);
        verify(imTalkService, never()).disable(any());
    }

    /**
     * 非会话参与方删除会话必须拒绝
     */
    @Test
    @DisplayName("删除会话-越权拦截")
    void disableThrowsWhenUnauthorized() {
        AuthUser authUser = new AuthUser("attacker", "user-999", "member", "test", UserEnums.MEMBER);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(authUser);

        ImTalk talk = new ImTalk();
        talk.setId("talk-101");
        talk.setUserId1("user-1");
        talk.setUserId2("store-1");
        when(imTalkService.getById("talk-101")).thenReturn(talk);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                imTalkController.disable("talk-101"));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(imTalkService, never()).disable(any());
    }
}
