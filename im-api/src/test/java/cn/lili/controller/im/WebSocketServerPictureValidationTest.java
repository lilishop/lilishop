package cn.lili.controller.im;

import cn.lili.cache.Cache;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.im.entity.dos.ImMessage;
import cn.lili.modules.im.entity.enums.OperationType;
import cn.lili.modules.im.entity.enums.MessageTypeEnum;
import cn.lili.modules.im.entity.vo.MessageOperation;
import cn.lili.modules.im.service.ImImageService;
import cn.lili.modules.im.service.ImMessageService;
import cn.lili.modules.im.service.ImTalkService;
import cn.lili.common.utils.SnowFlake;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import tools.jackson.databind.json.JsonMapper;
import static org.mockito.Mockito.*;

/**
 * WebSocket 客服图片消息发送校验与拦截单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@ExtendWith(MockitoExtension.class)
class WebSocketServerPictureValidationTest {

    @Mock
    private ImMessageService imMessageService;

    @Mock
    private ImTalkService imTalkService;

    @Mock
    private ImImageService imImageService;

    @Mock
    private Cache cache;

    private WebSocketServer webSocketServer;

    private MockedStatic<UserContext> userContextMock;

    @BeforeAll
    static void initSnowflake() {
        SnowFlake.initialize(1, 1);
    }

    @BeforeEach
    void setUp() {
        webSocketServer = new WebSocketServer(imMessageService, imTalkService, imImageService, cache, JsonMapper.builder().build());
        userContextMock = mockStatic(UserContext.class);
    }

    @AfterEach
    void tearDown() {
        if (userContextMock != null) {
            userContextMock.close();
        }
    }

    @Test
    @DisplayName("图片消息校验失败-终止保存并不投递消息")
    void pictureMessageValidationFailsAbortsSave() {
        AuthUser authUser = new AuthUser();
        authUser.setId("user-1");
        authUser.setRole(UserEnums.MEMBER);

        userContextMock.when(() -> UserContext.getAuthUser(cache, "token-1")).thenReturn(authUser);

        MessageOperation op = new MessageOperation();
        op.setOperationType(OperationType.MESSAGE.name());
        op.setMessageType(MessageTypeEnum.PICTURE.name());
        op.setTalkId("1001");
        op.setFrom("user-1");
        op.setTo("store-1");
        op.setContext("https://external.com/unauthorized.png");

        doThrow(new ServiceException("图片未上传到当前会话"))
                .when(imImageService).validateMessage(eq(authUser), eq(op));

        ReflectionTestUtils.invokeMethod(webSocketServer, "operation", "token-1", op);

        verify(imImageService).validateMessage(authUser, op);
        verify(imMessageService, never()).save(any(ImMessage.class));
        verify(imTalkService, never()).updateLastTalkMessage(any(), any(), any(), any());
    }

    @Test
    @DisplayName("图片消息校验成功-正常保存并更新会话")
    void pictureMessageValidationSuccessSavesMessage() {
        AuthUser authUser = new AuthUser();
        authUser.setId("user-1");
        authUser.setRole(UserEnums.MEMBER);

        userContextMock.when(() -> UserContext.getAuthUser(cache, "token-1")).thenReturn(authUser);

        MessageOperation op = new MessageOperation();
        op.setOperationType(OperationType.MESSAGE.name());
        op.setMessageType(MessageTypeEnum.PICTURE.name());
        op.setTalkId("1001");
        op.setFrom("user-1");
        op.setTo("store-1");
        op.setContext("https://im-bucket.oss.com/valid.png");

        doNothing().when(imImageService).validateMessage(eq(authUser), eq(op));

        ReflectionTestUtils.invokeMethod(webSocketServer, "operation", "token-1", op);

        verify(imImageService).validateMessage(authUser, op);
        verify(imMessageService).save(any(ImMessage.class));
        verify(imTalkService).updateLastTalkMessage(eq("1001"), eq("https://im-bucket.oss.com/valid.png"), any(), eq(MessageTypeEnum.PICTURE.name()));
    }
}
