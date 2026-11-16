package cn.lili.common.security.filter;

import jakarta.servlet.http.HttpServletRequest;
import cn.lili.modules.payment.kit.core.kit.HttpKit;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import java.nio.charset.StandardCharsets;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.Signature;
import java.util.Base64;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 微信通知过滤边界回归测试，验证原始签名报文可验签且普通接口继续防护 XSS。
 *
 * @author Bulbasaur
 * @since 2026-11-16
 */
class WechatNotificationXssFilterTest {

    /**
     * 使用本地 RSA 签名验证支付、退款和转账通知通过过滤链后头部及报文字节未改变。
     * 同时覆盖应用 context path，且篡改报文仍不能通过签名验证。
     *
     * @param path 微信通知精确路径
     * @throws Exception 本地签名或过滤链处理失败
     */
    @ParameterizedTest
    @ValueSource(strings = {"/buyer/payment/cashier/notify/WECHAT",
            "/buyer/payment/cashierRefund/notify/WECHAT", "/buyer/payment/cashier/transfer/notify/WECHAT"})
    void preservesSignedNotification(String path) throws Exception {
        String body = "{\r\n  \"id\": \"test-event\",\n  \"resource\": {\"ciphertext\": \"ab+c/d==\"}\r\n}\n";
        String timestamp = "1794783540";
        String nonce = "test+nonce=";
        KeyPairGenerator generator = KeyPairGenerator.getInstance("RSA");
        generator.initialize(2048);
        KeyPair keys = generator.generateKeyPair();
        Signature signer = Signature.getInstance("SHA256withRSA");
        signer.initSign(keys.getPrivate());
        signer.update(signatureMessage(timestamp, nonce, body));
        String signature = Base64.getEncoder().encodeToString(signer.sign());

        MockHttpServletRequest request = new MockHttpServletRequest("POST", "/api" + path);
        request.setContextPath("/api");
        request.setContentType("application/json");
        request.setContent(body.getBytes(StandardCharsets.UTF_8));
        request.addHeader("Wechatpay-Signature", signature);
        request.addHeader("Wechatpay-Timestamp", timestamp);
        request.addHeader("Wechatpay-Nonce", nonce);
        request.addHeader("Wechatpay-Serial", "test-serial");

        new XssFilter().doFilter(request, new MockHttpServletResponse(), (filtered, response) -> {
            assertThat(filtered).isSameAs(request);
            HttpServletRequest notification = (HttpServletRequest) filtered;
            assertThat(notification.getHeader("Wechatpay-Signature")).isEqualTo(signature);
            assertThat(notification.getHeader("Wechatpay-Serial")).isEqualTo("test-serial");
            byte[] received = HttpKit.readData(notification).getBytes(StandardCharsets.UTF_8);
            assertThat(received).isEqualTo(body.getBytes(StandardCharsets.UTF_8));
            try {
                Signature verifier = Signature.getInstance("SHA256withRSA");
                verifier.initVerify(keys.getPublic());
                verifier.update(signatureMessage(notification.getHeader("Wechatpay-Timestamp"),
                        notification.getHeader("Wechatpay-Nonce"), new String(received, StandardCharsets.UTF_8)));
                byte[] signatureBytes = Base64.getDecoder().decode(notification.getHeader("Wechatpay-Signature"));
                assertThat(verifier.verify(signatureBytes)).isTrue();
                verifier.update(signatureMessage(timestamp, nonce, body + " "));
                assertThat(verifier.verify(signatureBytes)).isFalse();
            } catch (java.security.GeneralSecurityException e) {
                throw new AssertionError("通知签名验证失败", e);
            }
        });
    }

    /**
     * 验证伪造微信签名头不能使普通接口、相似路径或其他渠道绕过 XSS 清洗。
     *
     * @param path 非白名单路径
     * @throws Exception 过滤链处理失败
     */
    @ParameterizedTest
    @ValueSource(strings = {"/buyer/member", "/buyer/payment/cashier/notify/WECHAT/extra",
            "/buyer/payment/cashier/notify/ALIPAY", "/buyer/payment/cashier/notify/WECHATevil"})
    void keepsOtherRequestsFiltered(String path) throws Exception {
        MockHttpServletRequest request = new MockHttpServletRequest("POST", path);
        request.addHeader("Wechatpay-Signature", "ab+c/d==");
        request.addParameter("content", "<script>alert(1)</script>");
        new XssFilter().doFilter(request, new MockHttpServletResponse(), (filtered, response) -> {
            assertThat(filtered).isInstanceOf(XssHttpServletRequestWrapper.class);
            assertThat(filtered.getParameter("content")).isEmpty();
        });
    }

    /**
     * 验证微信通知路径仅对 POST 保留报文，其他方法沿用普通过滤策略。
     *
     * @param method 非通知请求方法
     * @throws Exception 过滤链处理失败
     */
    @ParameterizedTest
    @ValueSource(strings = {"GET", "PUT", "DELETE"})
    void keepsOtherMethodsFiltered(String method) throws Exception {
        MockHttpServletRequest request = new MockHttpServletRequest(method, "/buyer/payment/cashier/notify/WECHAT");
        new XssFilter().doFilter(request, new MockHttpServletResponse(), (filtered, response) ->
                assertThat(filtered).isInstanceOf(XssHttpServletRequestWrapper.class));
    }

    /**
     * 构造微信通知的签名原文，保留请求体内及末尾的换行。
     *
     * @param timestamp 通知时间戳（秒）
     * @param nonce 通知随机串
     * @param body 原始请求体
     * @return UTF-8 签名原文字节
     */
    private byte[] signatureMessage(String timestamp, String nonce, String body) {
        return (timestamp + "\n" + nonce + "\n" + body + "\n").getBytes(StandardCharsets.UTF_8);
    }
}
