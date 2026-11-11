package cn.lili.common.security.context;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.SecurityEnum;
import cn.lili.common.security.token.SecretKeyUtil;
import cn.lili.common.utils.SpringContextUtil;
import cn.lili.common.utils.StringUtils;
import com.google.gson.Gson;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

/**
 * 用户上下文
 *
 * @author Chopper
 * @version v4.0
 * @since 2020/11/14 20:27
 */
public class UserContext {

    /**
     * 根据request获取用户信息（已校验服务端会话有效性）
     *
     * @return 授权用户；未登录或会话已失效时返回 null
     */
    public static AuthUser getCurrentUser() {
        // 1. 优先尝试从 Spring Security 上下文获取（已由 Filter 完成 Redis 会话强校验）
        if (SecurityContextHolder.getContext() != null) {
            Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
            if (authentication != null && authentication.getDetails() instanceof AuthUser authUser) {
                return authUser;
            }
        }
        // 2. 若未经过过滤器（如白名单免登录接口），从请求头解析并强制校验 Redis 存活态
        if (RequestContextHolder.getRequestAttributes() != null) {
            HttpServletRequest request = ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes()).getRequest();
            String accessToken = request.getHeader(SecurityEnum.HEADER_TOKEN.getValue());
            if (StringUtils.isNotEmpty(accessToken)) {
                return getAuthUserWithCache(accessToken);
            }
        }
        return null;
    }

    /**
     * 根据request获取用户信息
     *
     * @return 授权用户
     */
    public static String getUuid() {
        if (RequestContextHolder.getRequestAttributes() != null) {
            HttpServletRequest request = ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes()).getRequest();
            return request.getHeader(SecurityEnum.UUID.getValue());
        }
        return null;
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
            AuthUser authUser = getAuthUser(accessToken);
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
        if (RequestContextHolder.getRequestAttributes() != null) {
            HttpServletRequest request = ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes()).getRequest();
            return request.getHeader(SecurityEnum.HEADER_TOKEN.getValue());
        }
        return null;
    }

    /**
     * 根据jwt获取token重的用户信息
     *
     * @param accessToken token
     * @return 授权用户
     */
    public static AuthUser getAuthUser(String accessToken) {
        try {
            //获取token的信息
            Claims claims =
                    Jwts.parser()
                            .verifyWith(SecretKeyUtil.generalKeyByDecoders())
                            .build()
                            .parseSignedClaims(accessToken)
                            .getPayload();
            //获取存储在claims中的用户信息
            String json = claims.get(SecurityEnum.USER_CONTEXT.getValue()).toString();
            return new Gson().fromJson(json, AuthUser.class);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * 根据访问令牌解析用户并校验 Redis 服务端会话存活态
     *
     * @param accessToken 访问令牌
     * @return 授权用户；若令牌非法、过期或在 Redis 中已被注销，则返回 null
     */
    private static AuthUser getAuthUserWithCache(String accessToken) {
        AuthUser authUser = getAuthUser(accessToken);
        if (authUser == null) {
            return null;
        }
        try {
            if (SpringContextUtil.getApplicationContext() != null) {
                Cache cache = SpringContextUtil.getBean(Cache.class);
                if (cache != null) {
                    if (!cache.hasKey(CachePrefix.ACCESS_TOKEN.getPrefix(authUser.getRole(), authUser.getId()) + accessToken)) {
                        return null;
                    }
                }
            }
        } catch (Exception ignored) {
        }
        return authUser;
    }

    /**
     * 写入邀请人信息
     */
    public static void settingInviter(String memberId, Cache cache) {
        if (RequestContextHolder.getRequestAttributes() != null) {
            HttpServletRequest request = ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes()).getRequest();
            //邀请人id
            String inviterId = request.getHeader(SecurityEnum.INVITER.getValue());
            if (StringUtils.isNotEmpty(inviterId)) {
                cache.put(CachePrefix.INVITER.getPrefix() + memberId, inviterId);
            }
        }
    }

}
