package cn.lili.modules.connect.request;

import cn.hutool.core.convert.Convert;
import cn.lili.cache.Cache;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.utils.UrlBuilder;
import cn.lili.modules.connect.config.AuthConfig;
import cn.lili.modules.connect.config.ConnectAuthEnum;
import cn.lili.modules.connect.entity.dto.AuthCallback;
import cn.lili.modules.connect.entity.dto.AuthResponse;
import cn.lili.modules.connect.entity.dto.AuthToken;
import cn.lili.modules.connect.entity.dto.ConnectAuthUser;
import cn.lili.modules.connect.entity.enums.AuthResponseStatus;
import cn.lili.modules.connect.entity.enums.AuthUserGender;
import cn.lili.modules.connect.exception.AuthException;
import cn.lili.modules.connect.util.GlobalAuthUtils;
import cn.lili.common.utils.HttpUtils;
import com.alibaba.fastjson.JSONObject;

import java.util.Map;

/**
 * qq登录
 *
 * @author yadong.zhang (yadong.zhang0415(a)gmail.com)
 * @author yangkai.shen (https://xkcoding.com)
 * @since 1.1.0
 */
public class BaseAuthQQRequest extends BaseAuthRequest {

    public BaseAuthQQRequest(AuthConfig config, Cache cache) {
        super(config, ConnectAuthEnum.QQ, cache);
    }

    @Override
    protected AuthToken getAccessToken(AuthCallback authCallback) {
        String response = doGetAuthorizationCode(authCallback.getCode());
        return getAuthToken(response);
    }

    @Override
    public AuthResponse refresh(AuthToken authToken) {
        String response = new HttpUtils(config.getHttpConfig()).get(refreshTokenUrl(authToken.getRefreshToken()));
        return AuthResponse.builder().code(AuthResponseStatus.SUCCESS.getCode()).data(getAuthToken(response)).build();
    }

    @Override
    protected ConnectAuthUser getUserInfo(AuthToken authToken) {
        String openId = this.getOpenId(authToken);
        String response = doGetUserInfo(authToken);
        JSONObject object = JSONObject.parseObject(response);
        if (object.getIntValue("ret") != 0) {
            throw new AuthException(object.getString("msg"));
        }
        String avatar = object.getString("figureurl_qq_2");
        if (StringUtils.isEmpty(avatar)) {
            avatar = object.getString("figureurl_qq_1");
        }

        String location = String.format("%s-%s", object.getString("province"), object.getString("city"));
        return ConnectAuthUser.builder()
                .rawUserInfo(object)
                .username(object.getString("nickname"))
                .nickname(object.getString("nickname"))
                .avatar(avatar)
                .location(location)
                .uuid(openId)
                .gender(AuthUserGender.getRealGender(object.getString("gender")))
                .token(authToken)
                .source(source.toString())
                .build();
    }

    /**
     * 获取QQ用户的OpenId，支持自定义是否启用查询unionid的功能，如果启用查询unionid的功能，
     * 那就需要开发者先通过邮件申请unionid功能，参考链接 {@see http://wiki.connect.qq.com/unionid%E4%BB%8B%E7%BB%8D}
     *
     * @param authToken 通过{@link BaseAuthQQRequest#getAccessToken(AuthCallback)}获取到的{@code authToken}
     * @return openId
     */
    private String getOpenId(AuthToken authToken) {
        String response = new HttpUtils(config.getHttpConfig()).get(UrlBuilder.fromBaseUrl("https://graph.qq.com/oauth2.0/me")
                .queryParam("access_token", authToken.getAccessToken())
                .queryParam("unionid", config.isUnionId() ? 1 : 0)
                .build());
        String removePrefix = response.replace("callback(", "");
        String removeSuffix = removePrefix.replace(");", "");
        String openId = removeSuffix.trim();
        JSONObject object = JSONObject.parseObject(openId);
        if (object.containsKey("error")) {
            throw new AuthException(object.get("error") + ":" + object.get("error_description"));
        }
        authToken.setOpenId(object.getString("openid"));
        if (object.containsKey("unionid")) {
            authToken.setUnionId(object.getString("unionid"));
        }
        return StringUtils.isEmpty(authToken.getUnionId()) ? authToken.getOpenId() : authToken.getUnionId();
    }

    /**
     * 返回获取userInfo的url
     *
     * @param authToken 用户授权token
     * @return 返回获取userInfo的url
     */
    @Override
    protected String userInfoUrl(AuthToken authToken) {
        return UrlBuilder.fromBaseUrl(source.userInfo())
                .queryParam("access_token", authToken.getAccessToken())
                .queryParam("oauth_consumer_key", config.getClientId())
                .queryParam("openid", authToken.getOpenId())
                .build();
    }

    private AuthToken getAuthToken(String response) {
        Map<String, String> accessTokenObject = GlobalAuthUtils.parseStringToMap(response);
        if (!accessTokenObject.containsKey("access_token") || accessTokenObject.containsKey("code")) {
            throw new AuthException(accessTokenObject.get("msg"));
        }
        return AuthToken.builder()
                .accessToken(accessTokenObject.get("access_token"))
                .expireIn(Convert.toInt(accessTokenObject.getOrDefault("expires_in", "0")))
                .refreshToken(accessTokenObject.get("refresh_token"))
                .build();
    }

    @Override
    public String authorize(String state) {
        return UrlBuilder.fromBaseUrl(super.authorize(state))
                .queryParam("scope", "get_user_info")
                .build();
    }
}
