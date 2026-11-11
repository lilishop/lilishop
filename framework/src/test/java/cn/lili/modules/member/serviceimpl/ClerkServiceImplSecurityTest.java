package cn.lili.modules.member.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.member.entity.dos.Clerk;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * 店员服务越权防护单元测试，覆盖商家跨店铺禁用/启用店员及详情查询的安全隔离校验。
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class ClerkServiceImplSecurityTest {

    private static final String CURRENT_STORE_ID = "store-current";
    private static final String OTHER_STORE_ID = "store-other";
    private static final String CLERK_ID = "clerk-100";

    private ClerkServiceImpl clerkService;

    @BeforeAll
    static void initEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, Clerk.class);
    }

    @BeforeEach
    void setUp() {
        clerkService = spy(new ClerkServiceImpl());
    }

    private AuthUser currentStoreUser() {
        return AuthUser.builder()
                .id("user-1")
                .storeId(CURRENT_STORE_ID)
                .role(UserEnums.STORE)
                .username("merchant")
                .build();
    }

    @Test
    @DisplayName("商家禁用其他店铺店员时抛出越权异常")
    void disableOtherStoreClerkThrowsAuthorityError() {
        Clerk otherClerk = new Clerk();
        otherClerk.setId(CLERK_ID);
        otherClerk.setStoreId(OTHER_STORE_ID);
        otherClerk.setStatus(true);
        otherClerk.setShopkeeper(false);

        doReturn(otherClerk).when(clerkService).getById(CLERK_ID);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(currentStoreUser());

            ServiceException exception = assertThrows(ServiceException.class,
                    () -> clerkService.disable(CLERK_ID, false));

            assertThat(exception.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        }
    }

    @Test
    @DisplayName("商家禁用本店店员成功更新状态")
    void disableOwnStoreClerkSuccess() {
        Clerk ownClerk = new Clerk();
        ownClerk.setId(CLERK_ID);
        ownClerk.setStoreId(CURRENT_STORE_ID);
        ownClerk.setStatus(true);
        ownClerk.setShopkeeper(false);

        doReturn(ownClerk).when(clerkService).getById(CLERK_ID);
        doReturn(true).when(clerkService).updateById(any(Clerk.class));

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(currentStoreUser());

            clerkService.disable(CLERK_ID, false);

            assertThat(ownClerk.getStatus()).isFalse();
            verify(clerkService).updateById(ownClerk);
        }
    }

    @Test
    @DisplayName("商家查询其他店铺店员详情时抛出越权异常")
    void getOtherStoreClerkThrowsAuthorityError() {
        Clerk otherClerk = new Clerk();
        otherClerk.setId(CLERK_ID);
        otherClerk.setStoreId(OTHER_STORE_ID);

        doReturn(otherClerk).when(clerkService).getById(CLERK_ID);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(currentStoreUser());

            ServiceException exception = assertThrows(ServiceException.class,
                    () -> clerkService.get(CLERK_ID));

            assertThat(exception.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        }
    }
}
