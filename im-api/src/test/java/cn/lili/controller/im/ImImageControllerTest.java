package cn.lili.controller.im;

import cn.lili.cache.Cache;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.im.service.ImImageService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.mock.web.MockMultipartFile;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 客服会话图片上传控制器单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@ExtendWith(MockitoExtension.class)
class ImImageControllerTest {

    @Mock
    private Cache cache;

    @Mock
    private ImImageService imImageService;

    @InjectMocks
    private ImImageController imImageController;

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

    @Test
    @DisplayName("上传图片-鉴权通过并转发服务层处理")
    void uploadDelegatesToService() {
        AuthUser user = new AuthUser();
        user.setId("user-1");
        user.setRole(UserEnums.MEMBER);

        userContextMock.when(() -> UserContext.getAuthUser(cache, "token-123")).thenReturn(user);

        MockMultipartFile file = new MockMultipartFile("file", "test.png", "image/png", new byte[]{1, 2, 3});
        when(imImageService.upload(user, "1001", file)).thenReturn("https://im-bucket.oss.com/test.png");

        ResultMessage<String> result = imImageController.upload("token-123", "1001", file);

        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getResult()).isEqualTo("https://im-bucket.oss.com/test.png");
        verify(imImageService).upload(user, "1001", file);
    }
}
