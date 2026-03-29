package cn.lili.common.security.context;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.utils.StringUtils;
import com.alibaba.ttl.TransmittableThreadLocal;
import org.slf4j.MDC;

import java.util.Optional;

/**
 * 用户上下文
 *
 * @author Chopper
 * @version v4.0
 * @since 2020/11/14 20:27
 */
public class UserContext {

    private static final ThreadLocal<AuthUser> USER_THREAD_LOCAL = new TransmittableThreadLocal<>();

    // 日志追踪的 Key
    private static final String TRACE_USER_ID = "userId";

    /**
     * 将用户信息塞入上下文（仅在网关/拦截器调用 1 次）
     */
    public static void set(AuthUser user) {
        if (user != null) {
            USER_THREAD_LOCAL.set(user);
            // 日志集成！把 userId 塞进底层日志系统！
            MDC.put(TRACE_USER_ID, user.getId());
        }
    }

    /**
     * 获取当前登录用户（O(1) 内存提取，性能拉满）
     */
    public static AuthUser getCurrentUser() {
        return USER_THREAD_LOCAL.get();
    }

    /**
     * 高频便捷方法：安全获取当前用户ID (防空指针)
     */
    public static String getCurrentUserId() {
        return Optional.ofNullable(USER_THREAD_LOCAL.get())
                .map(AuthUser::getId)
                .orElse(null);
    }

    /**
     * 彻底清除上下文（必须在请求结束时调用，防内存泄漏与串号！）
     */
    public static void clear() {
        USER_THREAD_LOCAL.remove();
        MDC.remove(TRACE_USER_ID);
    }

    /*
      发现灾难在哪了吗？！

      JWT（JSON Web Token）的解析（parseClaimsJws）是带有**密码学签名校验（HMAC / RSA）**的！这可是极其消耗 CPU 算力的数学运算！

      假设你的一个业务请求进来，Controller 里调了一次 getCurrentUser() 拿用户 ID 去查表；Service 里调了一次拿角色去校验权限；
      最后操作日志切面（AOP）又调了一次拿用户名记日志。

      结果：这一个请求，把同一个 JWT 字符串，反反复复解密、校验了 3 次！！！

      如果双十一大促，每秒 5000 个并发打进来，光是这段“假马甲”代码里面重复执行的 JWT 密码学解密，就能把你们服务器的 CPU 瞬间干到 100% 死机！
      这就叫**“不仅没用，还成了性能刺客”

      @return 授权用户
     */

    /**
     * 根据request获取用户信息
     *
     * @return 授权用户
     */
    public static String getUuid() {
        return getCurrentUser().getUuid() == null ? null : getCurrentUser().getUuid();
    }


    /**
     * 根据jwt获取token重的用户信息
     *
     * @param cache       缓存
     * @param accessToken token
     * @return 授权用户
     */
    public static AuthUser getAuthUser(Cache cache, String accessToken) {
        try {
            AuthUser authUser = getAuthUser();
            assert authUser != null;

            if (!cache.hasKey(CachePrefix.ACCESS_TOKEN.getPrefix(authUser.getRole(), authUser.getId()) + accessToken)) {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
            return authUser;
        } catch (Exception e) {
            return null;
        }
    }

    public static String getCurrentUserToken() {
        return getCurrentUser().getAccessToken() == null ? null : getCurrentUser().getAccessToken();
    }

    /**
     * 根据jwt获取token重的用户信息
     *
     * @return 授权用户
     */
    public static AuthUser getAuthUser() {
        try {
            return getCurrentUser();
        } catch (Exception e) {
            return null;
        }
    }


    /**
     * 写入邀请人信息
     */
    public static void settingInviter(String memberId, Cache cache) {
        if (StringUtils.isNotEmpty(getCurrentUser().getInviter())) {
            cache.put(CachePrefix.INVITER.getPrefix() + memberId, getCurrentUser().getInviter());
        }
    }


}
