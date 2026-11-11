package cn.lili.common.security.support;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.enums.UserEnums;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.concurrent.TimeUnit;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 登录失败限流与防爆破锁定支持组件单元测试
 * 验证密码错误次数计数、达到阈值临时锁定及成功清除机制
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@ExtendWith(MockitoExtension.class)
public class LoginFailLimitSupportTest {

    @Mock
    private Cache cache;

    @InjectMocks
    private LoginFailLimitSupport loginFailLimitSupport;

    private final String username = "test_user";
    private final String lockKey = CachePrefix.LOGIN_TIME_LIMIT.getPrefix(UserEnums.MEMBER) + username;

    @Test
    @DisplayName("测试未达到重试上限时不触发锁定")
    public void testCheckLoginLockUnderLimit() {
        // 缓存中未有失败记录
        when(cache.get(lockKey)).thenReturn(null);
        assertDoesNotThrow(() -> loginFailLimitSupport.checkLoginLock(UserEnums.MEMBER, username));

        // 缓存中有3次失败记录（未达5次）
        when(cache.get(lockKey)).thenReturn(3);
        assertDoesNotThrow(() -> loginFailLimitSupport.checkLoginLock(UserEnums.MEMBER, username));
    }

    @Test
    @DisplayName("测试达到重试上限(5次)时抛出锁定异常阻止继续登录")
    public void testCheckLoginLockOverLimit() {
        when(cache.get(lockKey)).thenReturn(5);

        ServiceException exception = assertThrows(ServiceException.class, () ->
                loginFailLimitSupport.checkLoginLock(UserEnums.MEMBER, username)
        );

        assertThat(exception.getResultCode()).isEqualTo(ResultCode.USER_PASSWORD_ERROR_LOCKED);
        assertThat(exception.getMessage()).contains("密码连续错误超过 5 次，账号已临时锁定");
    }

    @Test
    @DisplayName("测试首次输错密码时记录失败次数并返回剩余可重试次数")
    public void testRecordPasswordFailFirstTime() {
        when(cache.get(lockKey)).thenReturn(0);

        ServiceException exception = assertThrows(ServiceException.class, () ->
                loginFailLimitSupport.recordPasswordFail(UserEnums.MEMBER, username)
        );

        // 校验错误码为密码不正确，剩余可尝试 4 次
        assertThat(exception.getResultCode()).isEqualTo(ResultCode.USER_PASSWORD_ERROR);
        assertThat(exception.getMessage()).contains("您还可以尝试 4 次");

        // 校验写入 Redis，设定 30 分钟过期
        verify(cache).put(eq(lockKey), eq(1), eq(30L), eq(TimeUnit.MINUTES));
    }

    @Test
    @DisplayName("测试输错密码达到第5次时直接触发锁定")
    public void testRecordPasswordFailReachLimit() {
        when(cache.get(lockKey)).thenReturn(4);

        ServiceException exception = assertThrows(ServiceException.class, () ->
                loginFailLimitSupport.recordPasswordFail(UserEnums.MEMBER, username)
        );

        // 校验错误码为账号锁定
        assertThat(exception.getResultCode()).isEqualTo(ResultCode.USER_PASSWORD_ERROR_LOCKED);
        assertThat(exception.getMessage()).contains("账号已锁定 30 分钟");

        // 校验写入 Redis 计数为 5
        verify(cache).put(eq(lockKey), eq(5), eq(30L), eq(TimeUnit.MINUTES));
    }

    @Test
    @DisplayName("测试清除失败计数时从 Redis 中删除对应锁定键")
    public void testClearLoginFail() {
        loginFailLimitSupport.clearLoginFail(UserEnums.MEMBER, username);
        verify(cache).remove(lockKey);
    }
}
