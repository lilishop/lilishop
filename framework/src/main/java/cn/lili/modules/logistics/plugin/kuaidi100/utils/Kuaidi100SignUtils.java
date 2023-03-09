package cn.lili.modules.logistics.plugin.kuaidi100.utils;

import org.apache.commons.codec.digest.DigestUtils;

/**
 * @Author: api.kuaidi100.com
 * @Date: 2020-07-14 16:54
 */
public class Kuaidi100SignUtils {

    /**
     * 快递100加密方式统一为MD5后转大写
     *
     * @param msg
     * @return
     */
    public static String sign(String msg) {
        return DigestUtils.md5Hex(msg).toUpperCase();
    }

    /**
     * 查询加密
     *
     * @param param
     * @param key
     * @param customer
     * @return
     */
    public static String querySign(String param, String key, String customer) {
        return sign(param + key + customer);
    }

    /**
     * 打印/下单 加密
     *
     * @param param
     * @param t
     * @param key
     * @param secret
     * @return: java.lang.String
     * @author: api.kuaidi100.com
     * @time: 2020/12/3 9:23
     */
    public static String printSign(String param, String t, String key, String secret) {
        return sign(param + t + key + secret);
    }

    /**
     * 云平台 加密
     *
     * @param key
     * @param secret
     * @return: java.lang.String
     * @author: api.kuaidi100.com
     * @time: 2020/12/3 9:23
     */
    public static String cloudSign(String key, String secret) {
        return sign(key + secret);
    }

    /**
     * 短信加密
     *
     * @param key
     * @param userId
     * @return: java.lang.String
     * @author: api.kuaidi100.com
     * @time: 2020/12/3 9:32
     */
    public static String smsSign(String key, String userId) {
        return sign(key + userId);
    }
}
