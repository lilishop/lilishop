package cn.lili.common.security.token;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.JWTTokenProperties;
import cn.lili.common.utils.SpringContextUtil;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.SecuritySetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.SecurityEnum;
import cn.lili.common.security.enums.UserEnums;
import com.google.gson.Gson;
import io.jsonwebtoken.*;
import io.jsonwebtoken.security.SignatureException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.concurrent.TimeUnit;

/**
 * TokenUtil
 *
 * @author Chopper
 * @version v1.0
 * 2020-11-12 18:44
 */
@Component
public class TokenUtil {
    @Autowired
    private JWTTokenProperties tokenProperties;
    @Autowired
    private Cache cache;

    private Long getAccessTokenExpireTime(AuthUser authUser) {
        if (authUser != null) {
            try {
                SettingService settingService = SpringContextUtil.getBean(SettingService.class);
                if (settingService != null) {
                    Setting setting = settingService.get(SettingEnum.SECURITY_SETTING.name());
                    if (setting != null && CharSequenceUtil.isNotBlank(setting.getSettingValue())) {
                        SecuritySetting securitySetting = JSONUtil.toBean(setting.getSettingValue(), SecuritySetting.class);
                        if (UserEnums.MEMBER.equals(authUser.getRole())) {
                            if (securitySetting.getBuyerTokenExpireTime() != null) {
                                return securitySetting.getBuyerTokenExpireTime();
                            }
                        } else {
                            if (securitySetting.getManagerTokenExpireTime() != null) {
                                return securitySetting.getManagerTokenExpireTime();
                            }
                        }
                    }
                }
            } catch (Exception ignored) {
            }
        }
        return tokenProperties != null ? tokenProperties.getTokenExpireTime() : 60L;
    }

    /**
     * 获取刷新令牌（RefreshToken）过期时间（单位：分钟）
     * 默认买家端为 30 天（43200 分钟），支持通过安全设置及配置文件扩展；其他端使用访问令牌的 2 倍时长
     *
     * @param authUser        授权用户
     * @param tokenExpireTime 当前访问令牌过期时间
     * @return 刷新令牌过期时间（分钟）
     */
    private Long getRefreshTokenExpireTime(AuthUser authUser, Long tokenExpireTime) {
        if (authUser != null) {
            try {
                SettingService settingService = SpringContextUtil.getBean(SettingService.class);
                if (settingService != null) {
                    Setting setting = settingService.get(SettingEnum.SECURITY_SETTING.name());
                    if (setting != null && CharSequenceUtil.isNotBlank(setting.getSettingValue())) {
                        SecuritySetting securitySetting = JSONUtil.toBean(setting.getSettingValue(), SecuritySetting.class);
                        if (UserEnums.MEMBER.equals(authUser.getRole()) && securitySetting.getBuyerRefreshTokenExpireTime() != null) {
                            return securitySetting.getBuyerRefreshTokenExpireTime();
                        }
                    }
                }
            } catch (Exception ignored) {
            }
            if (authUser.getLongTerm() || UserEnums.MEMBER.equals(authUser.getRole())) {
                return tokenProperties != null && tokenProperties.getRefreshTokenExpireTime() > 0
                        ? tokenProperties.getRefreshTokenExpireTime()
                        : 30 * 24 * 60L;
            }
        }
        return tokenExpireTime != null ? tokenExpireTime * 2 : 120L;
    }

    /**
     * 构建token
     *
     * @param authUser 私有声明
     * @return TOKEN
     */
    public Token createToken(AuthUser authUser) {
        Token token = new Token();
        Long tokenExpireTime = getAccessTokenExpireTime(authUser);
        //访问token
        String accessToken = createToken(authUser, tokenExpireTime);

        cache.put(CachePrefix.ACCESS_TOKEN.getPrefix(authUser.getRole(), authUser.getId()) + accessToken, 1,
                tokenExpireTime, TimeUnit.MINUTES);
        //刷新token生成策略：买家端或长期有效设备默认30天，其他角色使用普通token2倍时长
        Long expireTime = getRefreshTokenExpireTime(authUser, tokenExpireTime);
        String refreshToken = createToken(authUser, expireTime);

        cache.put(CachePrefix.REFRESH_TOKEN.getPrefix(authUser.getRole(), authUser.getId()) + refreshToken, 1, expireTime, TimeUnit.MINUTES);

        token.setAccessToken(accessToken);
        token.setRefreshToken(refreshToken);
        return token;
    }

    /**
     * 刷新token
     *
     * @param oldRefreshToken 刷新token
     * @return token
     */
    public Token refreshToken(String oldRefreshToken) {

        Claims claims;
        try {
            claims = Jwts.parser()
                    .verifyWith(SecretKeyUtil.generalKeyByDecoders())
                    .build()
                    .parseSignedClaims(oldRefreshToken)
                    .getPayload();
        } catch (ExpiredJwtException | UnsupportedJwtException | MalformedJwtException | SignatureException |
                 IllegalArgumentException e) {
            //token 过期 认证失败等
            throw new ServiceException(ResultCode.USER_AUTH_EXPIRED);
        }

        //获取存储在claims中的用户信息
        String json = claims.get(SecurityEnum.USER_CONTEXT.getValue()).toString();
        AuthUser authUser = new Gson().fromJson(json, AuthUser.class);
        UserEnums userEnums = authUser.getRole();

        String username = authUser.getUsername();
        //获取是否长期有效的token
        boolean longTerm = authUser.getLongTerm();

        //如果缓存中有刷新token &&
        if (cache.hasKey(CachePrefix.REFRESH_TOKEN.getPrefix(userEnums, authUser.getId()) + oldRefreshToken)) {
            Token token = new Token();
            Long tokenExpireTime = getAccessTokenExpireTime(authUser);
            //访问token
            String accessToken = createToken(authUser, tokenExpireTime);
            cache.put(CachePrefix.ACCESS_TOKEN.getPrefix(userEnums, authUser.getId()) + accessToken, 1, tokenExpireTime,
                    TimeUnit.MINUTES);

            Long expirationTime = getRefreshTokenExpireTime(authUser, tokenExpireTime);
            if (longTerm) {
                authUser.setLongTerm(true);
            }

            //刷新token生成策略：买家端或长期有效设备默认30天，其他角色使用普通token2倍时长
            String refreshToken = createToken(authUser, expirationTime);

            cache.put(CachePrefix.REFRESH_TOKEN.getPrefix(userEnums, authUser.getId()) + refreshToken, 1, expirationTime, TimeUnit.MINUTES);
            token.setAccessToken(accessToken);
            token.setRefreshToken(refreshToken);
            cache.remove(CachePrefix.REFRESH_TOKEN.getPrefix(userEnums, authUser.getId()) + oldRefreshToken);
            return token;
        } else {
            throw new ServiceException(ResultCode.USER_AUTH_EXPIRED);
        }

    }

    /**
     * 生成token
     *
     * @param authUser       jwt主体对象
     * @param expirationTime 过期时间（分钟）
     * @return token字符串
     */
    private String createToken(AuthUser authUser, Long expirationTime) {
        //JWT 生成
        return Jwts.builder()
                //jwt 私有声明
                .claim(SecurityEnum.USER_CONTEXT.getValue(), new Gson().toJson(authUser))
                //JWT的主体
                .subject(authUser.getUsername())
                //失效时间 当前时间+过期分钟
                .expiration(new Date(System.currentTimeMillis() + expirationTime * 60 * 1000))
                //签名算法和密钥
                .signWith(SecretKeyUtil.generalKey())
                .compact();
    }
}
