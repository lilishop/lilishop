
package cn.lili.modules.payment.kit.core;

import cn.hutool.core.collection.CollectionUtil;
import cn.hutool.core.map.CaseInsensitiveMap;
import cn.hutool.core.util.StrUtil;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * 支付接口响应
 *
 * @author Chopper
 * @since 2020/12/18 15:13
 */
public class PaymentHttpResponse implements Serializable {
    private static final long serialVersionUID = 6089103955998013402L;
    private String body;
    private int status;
    private Map<String, List<String>> headers;

    public String getBody() {
        return body;
    }

    public void setBody(String body) {
        this.body = body;
    }

    public int getStatus() {
        return status;
    }

    public void setStatus(int status) {
        this.status = status;
    }

    public Map<String, List<String>> getHeaders() {
        return headers;
    }

    public void setHeaders(Map<String, List<String>> headers) {
        this.headers = headers;
    }

    public String getHeader(String name) {
        List<String> values = this.headerList(name);
        return CollectionUtil.isEmpty(values) ? null : values.get(0);
    }

    private List<String> headerList(String name) {
        if (StrUtil.isBlank(name)) {
            return null;
        } else {
            CaseInsensitiveMap<String, List<String>> headersIgnoreCase = new CaseInsensitiveMap<>(getHeaders());
            return headersIgnoreCase.get(name.trim());
        }
    }

    @Override
    public String toString() {
        return "IJPayHttpResponse{" +
                "body='" + body + '\'' +
                ", status=" + status +
                ", headers=" + headers +
                '}';
    }
}
