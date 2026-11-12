package cn.lili.modules.payment.kit.plugin.alipay;

import jakarta.servlet.http.HttpServletRequest;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.io.UnsupportedEncodingException;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

/**
 * 支付宝接口门面及工具单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
class AliPayApiTest {

    @Test
    @DisplayName("测试异步通知参数转换为Map")
    void testToMap() {
        HttpServletRequest request = Mockito.mock(HttpServletRequest.class);
        Map<String, String[]> paramMap = new LinkedHashMap<>();
        paramMap.put("trade_no", new String[]{"20261112001"});
        paramMap.put("tags", new String[]{"tag1", "tag2"});
        paramMap.put("content", new String[]{"&lt;test&gt;"});

        when(request.getParameterMap()).thenReturn(paramMap);

        Map<String, String> result = AliPayApi.toMap(request);

        assertEquals("20261112001", result.get("trade_no"));
        assertEquals("tag1,tag2", result.get("tags"));
        assertEquals("<test>", result.get("content"));
    }

    @Test
    @DisplayName("测试应用授权URL生成")
    void testGetOauth2Url() throws UnsupportedEncodingException {
        String appId = "20210001";
        String redirectUri = "https://www.example.com/callback?foo=bar";

        String oauth2Url = AliPayApi.getOauth2Url(appId, redirectUri);

        assertTrue(oauth2Url.startsWith("https://openauth.alipay.com/oauth2/appToAppAuth.htm?app_id=20210001&redirect_uri="));
        assertTrue(oauth2Url.contains("https%3A%2F%2Fwww.example.com%2Fcallback%3Ffoo%3Dbar"));
    }
}
