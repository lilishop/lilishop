package cn.lili.common.security.filter;

import org.springframework.stereotype.Component;
import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.FilterConfig;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.annotation.WebFilter;
import jakarta.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.Set;

/**
 * 防止XSS攻击过滤器
 *
 * @author Chopper
 * @version v1.0
 * 2021-06-04 10:37
 */
@WebFilter
@Component
public class XssFilter implements Filter {
    /** 微信通知必须保留原始签名头和报文，可信性由支付 SDK 验签及后续业务校验保证。 */
    private static final Set<String> WECHAT_NOTIFICATION_PATHS = Set.of(
            "/buyer/payment/cashier/notify/WECHAT",
            "/buyer/payment/cashierRefund/notify/WECHAT",
            "/buyer/payment/cashier/transfer/notify/WECHAT"
    );

    FilterConfig filterConfig = null;

    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
        this.filterConfig = filterConfig;
    }

    /**
     * 普通请求执行 XSS 清洗；微信 POST 通知原样交给 SDK 验签、解密。
     * 签名覆盖原始报文，不能对签名头做 HTML 转义或重新序列化请求体；
     * 此处仅按精确路径保留报文，不依据可伪造的签名头放行其他业务接口。
     *
     * @param request 请求
     * @param response 响应
     * @param chain 后续过滤链
     * @throws IOException 过滤链读写失败
     * @throws ServletException 过滤链处理失败
     */
    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        String path = httpRequest.getRequestURI().substring(httpRequest.getContextPath().length());
        if ("POST".equals(httpRequest.getMethod()) && WECHAT_NOTIFICATION_PATHS.contains(path)) {
            chain.doFilter(request, response);
            return;
        }
        //对请求进行拦截,防xss处理
        chain.doFilter(new XssHttpServletRequestWrapper(httpRequest), response);
    }

    @Override
    public void destroy() {
        this.filterConfig = null;
    }
}
