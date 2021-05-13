package cn.lili.modules.connect.util;

import cn.hutool.json.JSONUtil;
import cn.lili.common.cache.Cache;
import cn.lili.common.cache.CachePrefix;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.token.Token;
import cn.lili.common.utils.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.config.properties.ApiProperties;
import cn.lili.config.properties.DomainProperties;
import cn.lili.modules.base.entity.enums.ClientTypeEnum;
import cn.lili.modules.connect.config.AuthConfig;
import cn.lili.modules.connect.config.ConnectAuthEnum;
import cn.lili.modules.connect.entity.dto.AuthCallback;
import cn.lili.modules.connect.entity.dto.AuthResponse;
import cn.lili.modules.connect.entity.dto.ConnectAuthUser;
import cn.lili.modules.connect.exception.AuthException;
import cn.lili.modules.connect.request.AuthRequest;
import cn.lili.modules.connect.request.AuthWeChatPCRequest;
import cn.lili.modules.connect.request.AuthWeChatRequest;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.connect.QQConnectSetting;
import cn.lili.modules.system.entity.dto.connect.WechatConnectSetting;
import cn.lili.modules.system.entity.dto.connect.dto.QQConnectSettingItem;
import cn.lili.modules.system.entity.dto.connect.dto.WechatConnectSettingItem;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * 联合登陆工具类
 *
 * @author Chopper
 * @version v1.0
 * 2020-11-25 21:16
 */
@Component
public class ConnectUtil {

    @Autowired
    private Cache cache;
    @Autowired
    private ConnectService connectService;
    @Autowired
    private SettingService settingService;
    @Autowired
    private ApiProperties apiProperties;
    @Autowired
    private DomainProperties domainProperties;


    static String prefix = "/buyer/connect/callback/";

    //回调地址获取
    String getRedirectUri(ConnectAuthEnum connectAuthEnum) {
        return apiProperties.getBuyer() + prefix + connectAuthEnum.getName();
    }

    /**
     * 登录回调
     *
     * @param type
     * @param callback
     * @param httpServletResponse
     * @throws IOException
     */
    public void callback(String type, AuthCallback callback, HttpServletResponse httpServletResponse) throws IOException {
        AuthRequest authRequest = this.getAuthRequest(type);
        AuthResponse<ConnectAuthUser> response = authRequest.login(callback);
        ResultMessage<Object> resultMessage;
        //联合登陆处理，如果响应正常，则录入响应结果到redis
        if (response.ok()) {
            ConnectAuthUser authUser = response.getData();
            Token token;
            try {
                token = connectService.unionLoginCallback(type, authUser, callback.getState());
                resultMessage = ResultUtil.data(token);
            } catch (ServiceException e) {
                resultMessage = ResultUtil.error(400,e.getMessage());
            }
        }
        //否则录入响应结果，等待前端获取信息
        else {
            resultMessage = ResultUtil.error(400,response.getMsg());
        }
        //缓存写入登录结果，300秒有效
        cache.put(CachePrefix.CONNECT_RESULT.getPrefix() + callback.getCode(), resultMessage, 300L);

//        String url = buyer + "/login?state=" + callback.getCode();
        String url = domainProperties.getWap() + "/pages/public/login?state=" + callback.getCode();
        try {
            httpServletResponse.sendRedirect(url);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * 获取响应结果
     *
     * @param state
     * @return
     */
    public ResultMessage<Object> getResult(String state) {
        Object object = cache.get(CachePrefix.CONNECT_RESULT.getPrefix() + state);
        if (object == null) {
            return null;
        } else {
            cache.remove(CachePrefix.CONNECT_RESULT.getPrefix() + state);
            return (ResultMessage<Object>) object;
        }
    }

    /**
     * 联合登录
     *
     * @param type 枚举
     * @return
     */
    public AuthRequest getAuthRequest(String type) {
        ConnectAuthEnum authInterface = ConnectAuthEnum.valueOf(type);
        if (authInterface == null) {
            throw new ServiceException("错误的登录方式");
        }
        AuthRequest authRequest = null;
        switch (authInterface) {
            case WECHAT: {
                //寻找配置
                Setting setting = settingService.get(SettingEnum.WECHAT_CONNECT.name());
                WechatConnectSetting wechatConnectSetting = JSONUtil.toBean(setting.getSettingValue(), WechatConnectSetting.class);

                for (WechatConnectSettingItem wechatConnectSettingItem : wechatConnectSetting.getWechatConnectSettingItems()) {
                    if (wechatConnectSettingItem.getClientType().equals(ClientTypeEnum.H5.name())) {
                        authRequest = new AuthWeChatRequest(AuthConfig.builder()
                                .clientId(wechatConnectSettingItem.getAppId())
                                .clientSecret(wechatConnectSettingItem.getAppSecret())
                                .redirectUri(getRedirectUri(authInterface))
                                .build(), cache);
                    }
                }
                break;
            }
            case WECHAT_PC: {
                //寻找配置
                Setting setting = settingService.get(SettingEnum.WECHAT_CONNECT.name());
                WechatConnectSetting wechatConnectSetting = JSONUtil.toBean(setting.getSettingValue(), WechatConnectSetting.class);
                for (WechatConnectSettingItem wechatConnectSettingItem : wechatConnectSetting.getWechatConnectSettingItems()) {
                    if (wechatConnectSettingItem.getClientType().equals(ClientTypeEnum.PC.name())) {
                        authRequest = new AuthWeChatPCRequest(AuthConfig.builder()
                                .clientId(wechatConnectSettingItem.getAppId())
                                .clientSecret(wechatConnectSettingItem.getAppSecret())
                                .redirectUri(getRedirectUri(authInterface))
                                .build(), cache);
                    }
                }

                break;
            }
            case QQ:

                //寻找配置
                Setting setting = settingService.get(SettingEnum.QQ_CONNECT.name());
                QQConnectSetting qqConnectSetting = JSONUtil.toBean(setting.getSettingValue(), QQConnectSetting.class);
                for (QQConnectSettingItem qqConnectSettingItem : qqConnectSetting.getQqConnectSettingItemList()) {
                    if (qqConnectSettingItem.getClientType().equals(ClientTypeEnum.PC.name())) {
                        authRequest = new AuthWeChatPCRequest(AuthConfig.builder()
                                .clientId(qqConnectSettingItem.getAppId())
                                .clientSecret(qqConnectSettingItem.getAppKey())
                                .redirectUri(getRedirectUri(authInterface))
                                .build(), cache);
                    }
                }

                break;
//            case ALIPAY:
//                // 支付宝在创建回调地址时，不允许使用localhost或者127.0.0.1，所以这儿的回调地址使用的局域网内的ip
//                authRequest = new AuthAlipayRequest(AuthConfig.builder()
//                        .clientId("")
//                        .clientSecret("")
//                        .alipayPublicKey("")
//                        .redirectUri(getRedirectUri(authInterface))
//                        .build(), cache);
//                break;
//            case WEIBO:
//                List<String> scopes = new ArrayList<>();
//                scopes.add("all");
//                authRequest = new AuthWeiboRequest(AuthConfig.builder()
//                        .clientId("")
//                        .clientSecret("")
//                        .redirectUri(getRedirectUri(authInterface))
//                        .scopes(scopes)
//                        .build(), cache);
//                break;
//            case "wechat_open":
//                authRequest = new AuthWeChatOpenRequest(AuthConfig.builder()
//                        .clientId("")
//                        .clientSecret("")
//                        .redirectUri("https://z171l91606.51mypc.cn/callback/wechat")
//                        .build());
//                break;
//            case "wechat_mp":
//                authRequest = new AuthWeChatMpRequest(AuthConfig.builder()
//                        .clientId("")
//                        .clientSecret("")
//                        .redirectUri("")
//                        .build());
//                break;
            default:
                break;
        }
        if (null == authRequest) {
            throw new AuthException("暂不支持第三方登陆");
        }
        return authRequest;
    }


}

