package cn.lili.modules.connect.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.filter.XssHttpServletRequestWrapper;
import cn.lili.modules.connect.entity.dto.WechatMPLoginParams;
import jakarta.servlet.http.HttpServletRequest;
import org.junit.jupiter.api.Test;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Base64;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 微信小程序登录解密回归测试，覆盖真实 XSS 清洗链路及非法授权失败行为。
 *
 * @author Bulbasaur
 * @since 2026-11-15
 */
class ConnectServiceImplTest {

    private final ConnectServiceImpl service = new ConnectServiceImpl();

    /**
     * 验证未经 HTML 转义的标准 Base64 授权数据仍可解密。
     */
    @Test
    void decryptsOriginalBase64() throws Exception {
        verifyDecryption(false);
    }

    /**
     * 验证请求包装器将加号和等号转义后仍能恢复并正确解密手机号。
     */
    @Test
    void decryptsAfterXssFiltering() throws Exception {
        verifyDecryption(true);
    }

    /**
     * 构造本地 AES 密文并验证标准参数或经过真实过滤器的参数，无需调用微信。
     *
     * @param filter 是否先经过 XSS 请求包装器
     * @throws Exception 本地加密失败
     */
    private void verifyDecryption(boolean filter) throws Exception {
        byte[] key = new byte[16];
        Arrays.fill(key, (byte) 0xfb);
        String keyBase64 = Base64.getEncoder().encodeToString(key);
        String iv = keyBase64;
        Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
        cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "AES"), new IvParameterSpec(key));
        String data = Base64.getEncoder().encodeToString(cipher.doFinal(
                "{\"purePhoneNumber\":\"13800138000\"}".getBytes(StandardCharsets.UTF_8)));
        if (filter) {
            HttpServletRequest request = mock(HttpServletRequest.class);
            when(request.getParameter("encryptedData")).thenReturn(data);
            when(request.getParameter("iv")).thenReturn(iv);
            XssHttpServletRequestWrapper wrapper = new XssHttpServletRequestWrapper(request);
            data = wrapper.getParameter("encryptedData");
            iv = wrapper.getParameter("iv");
            assertTrue(iv.contains("&#43;"));
            assertTrue(iv.contains("&#61;"));
        }
        assertEquals("13800138000", service.getUserInfo(data, keyBase64, iv).getString("purePhoneNumber"));
    }

    /**
     * 验证实体编码的脚本内容不能作为授权密文通过严格 Base64 校验。
     */
    @Test
    void rejectsEncodedScript() {
        ServiceException exception = assertThrows(ServiceException.class, () -> service.getUserInfo(
                "&lt;script&gt;alert(1)&lt;/script&gt;", "AAAAAAAAAAAAAAAAAAAAAA==", "AAAAAAAAAAAAAAAAAAAAAA=="));
        assertEquals(ResultCode.USER_CONNECT_ERROR, exception.getResultCode());
    }

    /**
     * 验证缺失或畸形的密文、密钥、向量统一返回授权错误，不泄漏底层编码异常。
     */
    @Test
    void rejectsInvalidParameters() {
        String valid = "AAAAAAAAAAAAAAAAAAAAAA==";
        String[][] inputs = {{null, valid, valid}, {valid, "invalid&", valid},
                {valid, valid, "invalid&"}, {valid, valid, valid}};
        for (String[] input : inputs) {
            ServiceException exception = assertThrows(ServiceException.class,
                    () -> service.getUserInfo(input[0], input[1], input[2]));
            assertEquals(ResultCode.USER_CONNECT_ERROR, exception.getResultCode());
        }
    }

    /**
     * 验证手机号登录在解密失败时抛出业务异常，避免控制器返回成功且 token 为空。
     */
    @Test
    void propagatesLoginFailure() {
        WechatMPLoginParams params = new WechatMPLoginParams();
        params.setEncryptedData("invalid&");
        params.setIv("AAAAAAAAAAAAAAAAAAAAAA==");
        ServiceException exception = assertThrows(ServiceException.class,
                () -> service.phoneMpBindAndLogin("AAAAAAAAAAAAAAAAAAAAAA==", params, "openid", "unionid"));
        assertEquals(ResultCode.USER_CONNECT_ERROR, exception.getResultCode());
    }
}
