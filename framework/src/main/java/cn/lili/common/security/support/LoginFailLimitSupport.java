package cn.lili.common.security.support;

import cn.hutool.core.convert.Convert;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.SpringContextUtil;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.SecuritySetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.concurrent.TimeUnit;

/**
 * 登录失败限流与防爆破锁定支持组件
 * 用于买家、商家、平台等用户体系在连续输错密码时进行计数与临时锁定
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@Slf4j
@Component
public class LoginFailLimitSupport {

    /**
     * 默认允许最大连续失败次数
     */
    public static final int DEFAULT_MAX_FAIL_COUNT = 5;

    /**
     * 默认账号锁定持续时长（单位：分钟）
     */
    public static final long DEFAULT_LOCK_MINUTES = 30L;

    @Autowired
    private Cache cache;

    /**
     * 构建失败计数缓存键
     *
     * @param userEnum 角色枚举
     * @param username 用户名
     * @return 缓存键
     */
    private String getLockKey(UserEnums userEnum, String username) {
        return CachePrefix.LOGIN_TIME_LIMIT.getPrefix(userEnum) + username;
    }

    /**
     * 登录前检查账号是否已被临时锁定
     *
     * @param userEnum 角色枚举
     * @param username 用户名
     * @throws ServiceException 当错误次数达到上限时抛出锁定异常
     */
    public void checkLoginLock(UserEnums userEnum, String username) {
        if (userEnum == null || CharSequenceUtil.isBlank(username)) {
            return;
        }
        String key = getLockKey(userEnum, username);
        Object value = cache.get(key);
        if (value != null) {
            int failCount = Convert.toInt(value, 0);
            int maxRetry = getMaxRetry();
            if (failCount >= maxRetry) {
                log.warn("用户登录被拦截：连续密码错误超限已被锁定，role={}, username={}", userEnum.name(), username);
                throw new ServiceException(ResultCode.USER_PASSWORD_ERROR_LOCKED,
                        "密码连续错误超过 " + maxRetry + " 次，账号已临时锁定，请稍后再试");
            }
        }
    }

    /**
     * 记录密码比对失败，增加失败计数并判断是否触发锁定
     *
     * @param userEnum 角色枚举
     * @param username 用户名
     * @throws ServiceException 抛出包含剩余重试次数或锁定提示的业务异常
     */
    public void recordPasswordFail(UserEnums userEnum, String username) {
        if (userEnum == null || CharSequenceUtil.isBlank(username)) {
            throw new ServiceException(ResultCode.USER_PASSWORD_ERROR);
        }
        String key = getLockKey(userEnum, username);
        int currentCount = Convert.toInt(cache.get(key), 0);
        int failCount = currentCount + 1;
        long lockMinutes = getLockMinutes();
        int maxRetry = getMaxRetry();

        // 写入最新失败次数并刷新锁定窗口过期时间
        cache.put(key, failCount, lockMinutes, TimeUnit.MINUTES);

        int remaining = maxRetry - failCount;
        if (remaining > 0) {
            log.warn("用户登录密码错误：role={}, username={}, currentFail={}, remaining={}",
                    userEnum.name(), username, failCount, remaining);
            throw new ServiceException(ResultCode.USER_PASSWORD_ERROR,
                    "密码不正确，您还可以尝试 " + remaining + " 次");
        } else {
            log.warn("用户登录触发锁定：role={}, username={}, totalFail={}, lockMinutes={}",
                    userEnum.name(), username, failCount, lockMinutes);
            throw new ServiceException(ResultCode.USER_PASSWORD_ERROR_LOCKED,
                    "密码连续错误超过 " + maxRetry + " 次，账号已锁定 " + lockMinutes + " 分钟");
        }
    }

    /**
     * 登录成功、找回密码或后台重置后清除失败计数与锁定状态
     *
     * @param userEnum 角色枚举
     * @param username 用户名
     */
    public void clearLoginFail(UserEnums userEnum, String username) {
        if (userEnum != null && CharSequenceUtil.isNotBlank(username)) {
            cache.remove(getLockKey(userEnum, username));
        }
    }

    /**
     * 获取最大允许重试次数（优先读取安全设置，默认5次）
     *
     * @return 最大重试次数
     */
    public int getMaxRetry() {
        SecuritySetting setting = getSecuritySetting();
        if (setting != null && setting.getPasswordMaxRetry() != null && setting.getPasswordMaxRetry() > 0) {
            return setting.getPasswordMaxRetry();
        }
        return DEFAULT_MAX_FAIL_COUNT;
    }

    /**
     * 获取超限锁定持续时长（优先读取安全设置，默认30分钟）
     *
     * @return 锁定分钟数
     */
    public long getLockMinutes() {
        SecuritySetting setting = getSecuritySetting();
        if (setting != null && setting.getPasswordLockMinutes() != null && setting.getPasswordLockMinutes() > 0) {
            return setting.getPasswordLockMinutes();
        }
        return DEFAULT_LOCK_MINUTES;
    }

    /**
     * 获取系统安全设置
     *
     * @return 安全配置实体
     */
    private SecuritySetting getSecuritySetting() {
        try {
            SettingService settingService = SpringContextUtil.getBean(SettingService.class);
            if (settingService != null) {
                Setting setting = settingService.get(SettingEnum.SECURITY_SETTING.name());
                if (setting != null && CharSequenceUtil.isNotBlank(setting.getSettingValue())) {
                    return JSONUtil.toBean(setting.getSettingValue(), SecuritySetting.class);
                }
            }
        } catch (Exception ignored) {
        }
        return null;
    }
}
