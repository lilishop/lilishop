package cn.lili.common.utils;

import lombok.extern.slf4j.Slf4j;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * CookieUtil
 *
 * @author Chopper
 * @version v1.0
 * 2020-12-14 09:32
 */
@Slf4j
public class CookieUtil {


    /**
     * 新增cookie
     *
     * @param key      key值
     * @param value    对应值
     * @param maxAge   cookie 有效时间
     * @param response 响应
     */
    public static void addCookie(String key, String value, Integer maxAge, HttpServletResponse response) {
        try {
            Cookie c = new Cookie(key, value);
            c.setMaxAge(maxAge);
            c.setPath("/");
            response.addCookie(c);
        } catch (Exception e) {
            log.error("新增cookie错误",e);
        }
    }

    /**
     * 删除cookie
     *
     * @param key      key值
     * @param response 响应
     */
    public static void delCookie(String key, HttpServletResponse response) {
        try {
            Cookie c = new Cookie(key, "");
            c.setMaxAge(0);
            response.addCookie(c);
        } catch (Exception e) {
            log.error("删除cookie错误",e);
        }
    }

    /**
     * 获取cookie
     *
     * @param key     key值
     * @param request 请求
     * @return cookie value
     */
    public static String getCookie(String key, HttpServletRequest request) {
        try {
            if (request.getCookies() == null) {
                return null;
            }
            for (int i = 0; i < request.getCookies().length; i++) {
                if (request.getCookies()[i].getName().equals(key)) {
                    return request.getCookies()[i].getValue();
                }
            }
        } catch (Exception e) {
            log.error("获取cookie错误",e);
        }
        return null;
    }
}
