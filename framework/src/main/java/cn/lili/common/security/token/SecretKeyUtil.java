package cn.lili.common.security.token;

import cn.lili.common.properties.JWTTokenProperties;
import cn.lili.common.utils.SpringContextUtil;
import io.jsonwebtoken.security.Keys;
import io.jsonwebtoken.io.Decoders;

import javax.crypto.SecretKey;

/**
 * SignWithUtil
 *
 * @author Chopper
 * @version v1.0
 * 2020-11-18 17:30
 */
public class SecretKeyUtil {
    public static SecretKey generalKey() {
        JWTTokenProperties tokenProperties = SpringContextUtil.getBean(JWTTokenProperties.class);
        return Keys.hmacShaKeyFor(Decoders.BASE64.decode(tokenProperties.getSecret()));
    }

    public static SecretKey generalKeyByDecoders() {
        return generalKey();
    }
}
