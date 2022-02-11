package cn.lili.common.utils;

import com.google.gson.Gson;
import lombok.extern.slf4j.Slf4j;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * response 输出响应工具
 *
 * @author Chopper
 */
@Slf4j
public class ResponseUtil {

    static final String ENCODING = "UTF-8";
    static final String CONTENT_TYPE = "application/json;charset=UTF-8";

    /**
     * 输出前端内容以及状态指定
     *
     * @param response
     * @param status
     * @param content
     */
    public static void output(HttpServletResponse response, Integer status, String content) {
        ServletOutputStream servletOutputStream = null;
        try {
            response.setCharacterEncoding(ENCODING);
            response.setContentType(CONTENT_TYPE);
            response.setStatus(status);
            servletOutputStream = response.getOutputStream();
            servletOutputStream.write(content.getBytes());
        } catch (Exception e) {
            log.error("response output error:", e);
        } finally {
            if (servletOutputStream != null) {
                try {
                    servletOutputStream.flush();
                    servletOutputStream.close();
                } catch (IOException e) {
                    log.error("response output IO close error:", e);
                }
            }
        }
    }


    /**
     * response 输出JSON
     *
     * @param response
     * @param status    response 状态
     * @param resultMap
     */
    public static void output(HttpServletResponse response, Integer status, Map<String, Object> resultMap) {
        response.setStatus(status);
        output(response, resultMap);
    }

    /**
     * response 输出JSON
     *
     * @param response
     * @param resultMap
     */
    public static void output(HttpServletResponse response, Map<String, Object> resultMap) {
        ServletOutputStream servletOutputStream = null;
        try {
            response.setCharacterEncoding(ENCODING);
            response.setContentType(CONTENT_TYPE);
            servletOutputStream = response.getOutputStream();
            servletOutputStream.write(new Gson().toJson(resultMap).getBytes());
        } catch (Exception e) {
            log.error("response output error:", e);
        } finally {
            if (servletOutputStream != null) {
                try {
                    servletOutputStream.flush();
                    servletOutputStream.close();
                } catch (IOException e) {
                    log.error("response output IO close error:", e);
                }
            }
        }
    }

    /**
     * 构造响应
     *
     * @param flag
     * @param code
     * @param msg
     * @return
     */
    public static Map<String, Object> resultMap(boolean flag, Integer code, String msg) {
        return resultMap(flag, code, msg, null);
    }

    /**
     * 构造响应
     *
     * @param flag
     * @param code
     * @param msg
     * @param data
     * @return
     */
    public static Map<String, Object> resultMap(boolean flag, Integer code, String msg, Object data) {

        Map<String, Object> resultMap = new HashMap<String, Object>(16);
        resultMap.put("success", flag);
        resultMap.put("message", msg);
        resultMap.put("code", code);
        resultMap.put("timestamp", System.currentTimeMillis());
        if (data != null) {
            resultMap.put("result", data);
        }
        return resultMap;
    }
}
