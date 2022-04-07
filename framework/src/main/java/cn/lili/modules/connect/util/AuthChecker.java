package cn.lili.modules.connect.util;


import cn.lili.cache.Cache;
import cn.lili.modules.connect.config.AuthConfig;
import cn.lili.modules.connect.config.ConnectAuth;
import cn.lili.modules.connect.config.ConnectAuthEnum;
import cn.lili.modules.connect.entity.dto.AuthCallback;
import cn.lili.modules.connect.entity.enums.AuthResponseStatus;
import cn.lili.modules.connect.exception.AuthException;
import org.apache.commons.lang3.StringUtils;


/**
 * 授权配置类的校验器
 *
 * @author yadong.zhang (yadong.zhang0415(a)gmail.com)
 * @since 1.6.1-beta
 */
public class AuthChecker {

    /**
     * 是否支持第三方登录
     *
     * @param config        config
     * @param connectAuth source
     * @return true or false
     * @since 1.6.1-beta
     */
    public static boolean isSupportedAuth(AuthConfig config, ConnectAuth connectAuth) {
        boolean isSupported = StringUtils.isNotEmpty(config.getClientId()) && StringUtils.isNotEmpty(config.getClientSecret()) && StringUtils.isNotEmpty(config.getRedirectUri());
        if (isSupported && ConnectAuthEnum.ALIPAY == connectAuth) {
            isSupported = StringUtils.isNotEmpty(config.getAlipayPublicKey());
        }
        return isSupported;
    }

    /**
     * 检查配置合法性。针对部分平台， 对redirect uri有特定要求。一般来说redirect uri都是http://，而对于facebook平台， redirect uri 必须是https的链接
     *
     * @param config        config
     * @param connectAuth source
     * @since 1.6.1-beta
     */
    public static void checkConfig(AuthConfig config, ConnectAuth connectAuth) {
        String redirectUri = config.getRedirectUri();
        if (!GlobalAuthUtils.isHttpProtocol(redirectUri) && !GlobalAuthUtils.isHttpsProtocol(redirectUri)) {
            throw new AuthException(AuthResponseStatus.ILLEGAL_REDIRECT_URI, connectAuth);
        }
        //支付宝在创建回调地址时，不允许使用localhost或者106.124.130.167
        if (ConnectAuthEnum.ALIPAY == connectAuth && GlobalAuthUtils.isLocalHost(redirectUri)) {
            //The redirect uri of alipay is forbidden to use localhost or 106.124.130.167
            throw new AuthException(AuthResponseStatus.ILLEGAL_REDIRECT_URI, connectAuth);
        }
    }

    /**
     * 校验回调传回的code
     * <p>
     * {@code v1.10.0}版本中改为传入{@code source}和{@code callback}，对于不同平台使用不同参数接受code的情况统一做处理
     *
     * @param connectAuth 当前授权平台
     * @param callback      从第三方授权回调回来时传入的参数集合
     * @since 1.8.0
     */
    public static void checkCode(ConnectAuth connectAuth, AuthCallback callback) {
        String code = callback.getCode();
        if (connectAuth == ConnectAuthEnum.ALIPAY) {
            code = callback.getAuthCode();
        }
        if (StringUtils.isEmpty(code)) {
            throw new AuthException(AuthResponseStatus.ILLEGAL_CODE, connectAuth);
        }
    }

    /**
     * 校验回调传回的{@code state}，为空或者不存在
     * <p>
     * {@code state}不存在的情况只有两种：
     * 1. {@code state}已使用，被正常清除
     * 2. {@code state}为前端伪造，本身就不存在
     *
     * @param state {@code state}一定不为空
     */
    public static void checkState(String state, ConnectAuth connectAuth, Cache cache) {
        if (StringUtils.isEmpty(state) || !cache.hasKey(state)) {
            throw new AuthException(AuthResponseStatus.ILLEGAL_STATUS, connectAuth);
        }
    }
}
