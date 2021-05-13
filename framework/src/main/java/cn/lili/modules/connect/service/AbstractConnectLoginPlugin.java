package cn.lili.modules.connect.service;

import cn.lili.common.utils.UrlBuilder;
import cn.lili.modules.connect.config.ConnectAuthEnum;
import cn.lili.modules.connect.entity.dto.AuthToken;
import org.springframework.stereotype.Component;


/**
 * 信任登录抽象类
 *
 * @author Chopper
 * @version v4.0
 * @Description:
 * @since 2020/12/4 10:57
 */
@Component
public abstract class AbstractConnectLoginPlugin {

    protected static String callBackUrl = "http://www.baidu.com";

    /**
     * 获取授权登录的url
     *
     * @return URL
     */
    protected abstract String getLoginUrl(String uuid);

    /**
     * 回调地址
     *
     * @return 登录凭证
     */
    protected String callbackUrl(String uuid, ConnectAuthEnum authInterface) {
        return UrlBuilder.fromBaseUrl(callBackUrl)
                .pathAppend("/buyer/connect/callback")
                .pathAppend("/" + authInterface.getName())
                .queryParam("uuid", uuid).build();
    }

    /**
     * 回调地址
     *
     * @return 登录凭证
     */
    protected abstract AuthToken userInfo(String uuid);

}
