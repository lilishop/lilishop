package cn.lili.controller.seller.permission;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.member.entity.dos.Clerk;
import cn.lili.modules.member.service.ClerkService;
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
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 商家端店员控制器越权防护单元测试，验证跨店铺启停店员时通过 OperationalJudgment 阻断。
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@ExtendWith(MockitoExtension.class)
class ClerkStoreControllerSecurityTest {

    private static final String CURRENT_STORE_ID = "store-current";
    private static final String OTHER_STORE_ID = "store-other";
    private static final String CLERK_ID = "clerk-100";

    @InjectMocks
    private ClerkStoreController controller;

    @Mock
    private ClerkService clerkService;

    private AuthUser currentStoreUser() {
        return AuthUser.builder()
                .id("user-1")
                .storeId(CURRENT_STORE_ID)
                .role(UserEnums.STORE)
                .username("merchant")
                .build();
    }

    @Test
    @DisplayName("商家启停其他店铺店员时在Controller层被拒绝")
    void disableOtherStoreClerkThrowsAuthorityError() {
        Clerk otherClerk = new Clerk();
        otherClerk.setId(CLERK_ID);
        otherClerk.setStoreId(OTHER_STORE_ID);

        when(clerkService.getById(CLERK_ID)).thenReturn(otherClerk);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(currentStoreUser());

            ServiceException exception = assertThrows(ServiceException.class,
                    () -> controller.disable(CLERK_ID, false));

            assertThat(exception.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
            verify(clerkService, never()).disable(any(), any());
        }
    }

    @Test
    @DisplayName("商家启停本店店员时正常调用业务服务")
    void disableOwnStoreClerkSuccess() {
        Clerk ownClerk = new Clerk();
        ownClerk.setId(CLERK_ID);
        ownClerk.setStoreId(CURRENT_STORE_ID);

        when(clerkService.getById(CLERK_ID)).thenReturn(ownClerk);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(currentStoreUser());

            controller.disable(CLERK_ID, false);

            verify(clerkService).disable(eq(CLERK_ID), eq(false));
        }
    }
}
