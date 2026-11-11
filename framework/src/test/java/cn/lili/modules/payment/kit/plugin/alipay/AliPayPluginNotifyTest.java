package cn.lili.modules.payment.kit.plugin.alipay;

import jakarta.servlet.http.HttpServletRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 支付宝支付异步通知处理测试
 * 验证处理成功向响应写入 success，验签或处理失败向响应写入 fail
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class AliPayPluginNotifyTest {

    private MockHttpServletRequest request;
    private MockHttpServletResponse response;

    @BeforeEach
    void setUp() {
        request = new MockHttpServletRequest();
        response = new MockHttpServletResponse();
    }

    /**
     * 验证通知处理成功时向 Response 写入 success
     */
    @Test
    @DisplayName("支付宝回调处理成功向Response写入success")
    void notifySuccessWritesSuccess() throws Exception {
        AliPayPlugin plugin = new AliPayPlugin() {
            @Override
            boolean verifyNotify(HttpServletRequest req) {
                return true;
            }
        };

        plugin.notify(request, response);

        assertThat(response.getStatus()).isEqualTo(200);
        assertThat(response.getContentType()).contains("text/plain");
        assertThat(response.getContentAsString()).isEqualTo("success");
    }

    /**
     * 验证通知处理失败（如验签失败或异常）时向 Response 写入 fail
     */
    @Test
    @DisplayName("支付宝回调处理失败向Response写入fail")
    void notifyFailureWritesFail() throws Exception {
        AliPayPlugin plugin = new AliPayPlugin() {
            @Override
            boolean verifyNotify(HttpServletRequest req) {
                return false;
            }
        };

        plugin.notify(request, response);

        assertThat(response.getStatus()).isEqualTo(200);
        assertThat(response.getContentType()).contains("text/plain");
        assertThat(response.getContentAsString()).isEqualTo("fail");
    }

    /**
     * 验证未传入 response 且无异常时能够平稳降级
     */
    @Test
    @DisplayName("支付宝回调未传入response时平稳处理")
    void notifyWithoutResponseDoesNotThrow() {
        AliPayPlugin plugin = new AliPayPlugin() {
            @Override
            boolean verifyNotify(HttpServletRequest req) {
                return true;
            }
        };

        plugin.notify(request, null);
    }
}
