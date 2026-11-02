package cn.lili.modules.wechat.util;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import lombok.extern.slf4j.Slf4j;

/**
 * 微信消息响应处理工具。
 * 场景化模板消息发送已由 {@link WechatSceneMessageSender} 接管。
 *
 * @author Chopper
 * @version v1.0
 * 2020-12-10 19:12
 */
@Slf4j
public class WechatMessageUtil {

    /**
     * 如果返回信息有错误
     *
     * @param jsonObject 返回消息
     */
    public static void wechatHandler(JSONObject jsonObject) {
        if (jsonObject.containsKey("errmsg")) {
            if (("ok").equals(jsonObject.getString("errmsg"))) {
                return;
            }
            log.error("微信接口异常，错误码" + jsonObject.getIntValue("errcode") + "，" + jsonObject.getString("errmsg"));
            throw new ServiceException(ResultCode.WECHAT_ERROR);
        }
    }

    /**
     * 如果返回信息有错误
     *
     * @param string 返回消息
     */
    public static String wechatHandler(String string) {
        JSONObject jsonObject = JSON.parseObject(string);
        wechatHandler(jsonObject);
        return string;
    }

}
