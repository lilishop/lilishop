package cn.lili.modules.payment.kit.core.kit;

import cn.lili.modules.payment.kit.core.http.AbstractHttpDelegate;
import lombok.extern.slf4j.Slf4j;

import jakarta.servlet.http.HttpServletRequest;
import java.io.BufferedReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * <p>Http 工具类</p>
 *
 * @author
 */
@Slf4j
public class HttpKit {

    private static AbstractHttpDelegate delegate = new DefaultHttpKit();

    public static AbstractHttpDelegate getDelegate() {
        return delegate;
    }

    public static void setDelegate(AbstractHttpDelegate delegate) {
        HttpKit.delegate = delegate;
    }

    /**
     * 读取支付通知原始文本，保留 CR、LF 和末尾换行，避免改变 SDK 验签原文。
     *
     * @param request 支付通知请求，字符集由 Servlet 请求配置决定
     * @return 未重新序列化或规范化换行的请求体
     * @throws RuntimeException 请求体读取失败
     */
    public static String readData(HttpServletRequest request) {
        BufferedReader br = null;
        try {
            StringBuilder result = new StringBuilder();
            br = request.getReader();
            char[] buffer = new char[4096];
            for (int length; (length = br.read(buffer)) != -1; ) {
                result.append(buffer, 0, length);
            }
            return result.toString();
        } catch (IOException e) {
            throw new RuntimeException(e);
        } finally {
            if (br != null) {
                try {
                    br.close();
                } catch (IOException e) {
                    log.error("readData错误",e);
                }
            }
        }
    }

    /**
     * 将同步通知的参数转化为Map
     *
     * @param request {@link HttpServletRequest}
     * @return 转化后的 Map
     */
    public static Map<String, String> toMap(HttpServletRequest request) {
        Map<String, String> params = new HashMap<>(16);
        Map<String, String[]> requestParams = request.getParameterMap();
        for (String name : requestParams.keySet()) {
            String[] values = requestParams.get(name);
            String valueStr = "";
            for (int i = 0; i < values.length; i++) {
                valueStr = (i == values.length - 1) ? valueStr + values[i] : valueStr + values[i] + ",";
            }
            params.put(name, valueStr);
        }
        return params;
    }
}

/**
 * 使用 huTool 实现的 Http 工具类
 *
 * @author
 */
class DefaultHttpKit extends AbstractHttpDelegate {
}
