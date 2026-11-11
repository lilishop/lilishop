package cn.lili.modules.payment.kit.plugin.wechat;

import com.wechat.pay.java.core.exception.ValidationException;
import jakarta.servlet.http.HttpServletRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 微信支付回调结果通知处理测试
 * 验证验签失败返回 HTTP 401、业务异常返回 HTTP 500、成功返回 HTTP 200 及标准化响应报文
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class WechatPluginNotifyTest {

    private MockHttpServletRequest request;
    private MockHttpServletResponse response;

    @BeforeEach
    void setUp() {
        request = new MockHttpServletRequest();
        response = new MockHttpServletResponse();
    }

    /**
     * 验证验签失败时返回 401 状态码且响应体包含 FAIL 与错误原因
     */
    @Test
    @DisplayName("微信回调验签失败返回HTTP 401")
    void notifyValidationExceptionReturns401() throws Exception {
        WechatPlugin plugin = new WechatPlugin() {
            @Override
            void verifyNotify(HttpServletRequest req) throws Exception {
                throw new ValidationException("签名验证失败");
            }
        };

        plugin.notify(request, response);

        assertThat(response.getStatus()).isEqualTo(401);
        assertThat(response.getContentType()).contains("application/json");
        assertThat(response.getContentAsString()).contains("\"code\":\"FAIL\"");
        assertThat(response.getContentAsString()).contains("签名验证失败");
    }

    /**
     * 验证业务处理发生异常时返回 HTTP 500 且响应体为 FAIL
     */
    @Test
    @DisplayName("微信回调业务异常返回HTTP 500")
    void notifyExecutionExceptionReturns500() throws Exception {
        WechatPlugin plugin = new WechatPlugin() {
            @Override
            void verifyNotify(HttpServletRequest req) throws Exception {
                throw new RuntimeException("数据库写入超时");
            }
        };

        plugin.notify(request, response);

        assertThat(response.getStatus()).isEqualTo(500);
        assertThat(response.getContentType()).contains("application/json");
        assertThat(response.getContentAsString()).contains("\"code\":\"FAIL\"");
        assertThat(response.getContentAsString()).contains("数据库写入超时");
    }

    /**
     * 验证成功处理时返回 HTTP 200 且响应体为 SUCCESS
     */
    @Test
    @DisplayName("微信回调正常处理返回HTTP 200")
    void notifySuccessReturns200() throws Exception {
        WechatPlugin plugin = new WechatPlugin() {
            @Override
            void verifyNotify(HttpServletRequest req) {
                // 正常处理成功
            }
        };

        plugin.notify(request, response);

        assertThat(response.getStatus()).isEqualTo(200);
        assertThat(response.getContentType()).contains("application/json");
        assertThat(response.getContentAsString()).contains("\"code\":\"SUCCESS\"");
        assertThat(response.getContentAsString()).contains("成功");
    }
}
