package cn.lili.common.security.context;

import cn.lili.common.cache.Cache;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.SecurityEnum;
import cn.lili.common.token.SecretKeyUtil;
import com.google.gson.Gson;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;

/**
 * 用户上下文
 *
 * @author Chopper
 * @version v4.0
 * @Description:
 * @since 2020/11/14 20:27
 */
public class UserContext {

    private static AuthenticationHandler authenticationHandler;

    public static void setHolder(AuthenticationHandler authenticationHandler) {
        UserContext.authenticationHandler = authenticationHandler;
    }


    public static AuthUser getCurrentUser() {
        return authenticationHandler.getAuthUser();
    }


    /**
     * 根据jwt获取token重的用户信息
     *
     * @param cache       缓存
     * @param accessToken token
     * @return
     */
    public static AuthUser getAuthUser(Cache cache, String accessToken) {
        try {
            if (cache.keys("*" + accessToken).size() == 0) {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
            //获取token的信息
            Claims claims
                    = Jwts.parser()
                    .setSigningKey(SecretKeyUtil.generalKeyByDecoders())
                    .parseClaimsJws(accessToken).getBody();
            //获取存储在claims中的用户信息
            String json = claims.get(SecurityEnum.USER_CONTEXT.getValue()).toString();
            return new Gson().fromJson(json, AuthUser.class);
        } catch (Exception e) {
            return null;
        }
    }
}
