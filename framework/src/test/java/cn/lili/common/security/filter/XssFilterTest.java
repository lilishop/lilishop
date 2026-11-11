package cn.lili.common.security.filter;

import cn.hutool.core.io.IoUtil;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import jakarta.servlet.ReadListener;
import jakarta.servlet.ServletInputStream;
import jakarta.servlet.http.HttpServletRequest;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * XSS 请求内容包装器及过滤器单元测试，验证防脚本注入、属性过滤及实体还原绕过防护。
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class XssFilterTest {

    /**
     * 验证不再执行反转义还原，防止攻击者通过 HTML 实体编码反向还原出可执行的恶意脚本标签。
     */
    @Test
    @DisplayName("防止通过 HTML 实体反向还原出恶意脚本")
    void preventsEntityBypassWithoutUnescaping() {
        HttpServletRequest request = mock(HttpServletRequest.class);
        String entityScript = "&lt;script&gt;alert('bypass')&lt;/script&gt;";
        String entityImg = "&lt;img src=x onerror=alert(1)&gt;";

        when(request.getParameter("scriptParam")).thenReturn(entityScript);
        when(request.getParameter("imgParam")).thenReturn(entityImg);

        XssHttpServletRequestWrapper wrapper = new XssHttpServletRequestWrapper(request);

        // 核心断言：绝对不能被反转义还原成包含可执行标签的危险字符串
        assertThat(wrapper.getParameter("scriptParam")).doesNotContain("<script>");
        assertThat(wrapper.getParameter("imgParam")).doesNotContain("<img");
    }

    /**
     * 验证原生 script 标签及脚本块内容整体被安全移除，事件属性及非安全伪协议均被清洗。
     */
    @Test
    @DisplayName("过滤危险标签、事件属性及伪协议")
    void filtersScriptTagsAndMaliciousAttributes() {
        HttpServletRequest request = mock(HttpServletRequest.class);
        when(request.getParameter("rawScript")).thenReturn("<script>alert('xss')</script>");
        when(request.getParameter("maliciousImg")).thenReturn("<img src=\"https://test.com/a.png\" onerror=\"alert('xss')\">");
        when(request.getParameter("jsLink")).thenReturn("<a href=\"javascript:alert('xss')\">点击</a>");

        XssHttpServletRequestWrapper wrapper = new XssHttpServletRequestWrapper(request);

        // OWASP Sanitizer 会彻底剔除 script 标签及其内部脚本体
        assertThat(wrapper.getParameter("rawScript")).isEmpty();
        // onerror 事件属性被剔除，安全图片属性保留
        assertThat(wrapper.getParameter("maliciousImg")).doesNotContain("onerror");
        assertThat(wrapper.getParameter("maliciousImg")).contains("src=\"https://test.com/a.png\"");
        // 非法伪协议 javascript: 被剔除
        assertThat(wrapper.getParameter("jsLink")).doesNotContain("javascript:");
        assertThat(wrapper.getParameter("jsLink")).contains("点击");
    }

    /**
     * 验证合法的安全 HTML 标签正常保留，不破坏富文本内容结构。
     */
    @Test
    @DisplayName("正常保留白名单允许的安全 HTML 标签")
    void preservesSafeHtmlElements() {
        HttpServletRequest request = mock(HttpServletRequest.class);
        String safeHtml = "<p>正文内容 <strong>加粗</strong></p>";
        when(request.getParameter("content")).thenReturn(safeHtml);

        XssHttpServletRequestWrapper wrapper = new XssHttpServletRequestWrapper(request);

        assertThat(wrapper.getParameter("content")).isEqualTo(safeHtml);
    }

    /**
     * 验证多值参数、参数映射及请求头均经过统一的安全清洗。
     */
    @Test
    @DisplayName("统一处理数组参数、参数映射及请求头")
    void handlesArrayParametersAndHeaders() {
        HttpServletRequest request = mock(HttpServletRequest.class);
        String[] values = new String[]{"<script>1</script>", "安全文本"};
        when(request.getParameterValues("arr")).thenReturn(values);

        Map<String, String[]> map = new HashMap<>();
        map.put("arr", new String[]{"<script>1</script>", "安全文本"});
        when(request.getParameterMap()).thenReturn(map);
        when(request.getHeader("X-Custom")).thenReturn("<script>header</script>");

        XssHttpServletRequestWrapper wrapper = new XssHttpServletRequestWrapper(request);

        String[] filteredValues = wrapper.getParameterValues("arr");
        assertThat(filteredValues).containsExactly("", "安全文本");

        Map<String, String[]> filteredMap = wrapper.getParameterMap();
        assertThat(filteredMap.get("arr")).containsExactly("", "安全文本");

        assertThat(wrapper.getHeader("X-Custom")).isEmpty();
    }

    /**
     * 验证 JSON 请求体输入流中的 XSS 内容被正确清洗且实体不会被反转义还原。
     */
    @Test
    @DisplayName("清洗 JSON RequestBody 输入流中的 XSS 载荷")
    void filtersJsonRequestBodyInInputStream() throws IOException {
        HttpServletRequest request = mock(HttpServletRequest.class);
        String json = "{\"title\":\"&lt;script&gt;test&lt;/script&gt;\",\"content\":\"<script>alert(1)</script>\",\"count\":10}";
        byte[] bytes = json.getBytes(StandardCharsets.UTF_8);

        when(request.getInputStream()).thenReturn(new DelegatingServletInputStream(bytes));

        XssHttpServletRequestWrapper wrapper = new XssHttpServletRequestWrapper(request);
        ServletInputStream in = wrapper.getInputStream();
        String resultJson = IoUtil.read(in, StandardCharsets.UTF_8);

        JSONObject obj = JSONUtil.parseObj(resultJson);
        assertThat(obj.getStr("content")).isEmpty();
        // 实体编码不能还原成真实的 <script>
        assertThat(obj.getStr("title")).doesNotContain("<script>");
        assertThat(obj.getInt("count")).isEqualTo(10);
    }

    /**
     * 测试用委托输入流，包装字节数组供 ServletInputStream 读取。
     */
    private static class DelegatingServletInputStream extends ServletInputStream {

        private final ByteArrayInputStream sourceStream;

        public DelegatingServletInputStream(byte[] data) {
            this.sourceStream = new ByteArrayInputStream(data);
        }

        @Override
        public boolean isFinished() {
            return sourceStream.available() == 0;
        }

        @Override
        public boolean isReady() {
            return true;
        }

        @Override
        public void setReadListener(ReadListener readListener) {
        }

        @Override
        public int read() {
            return sourceStream.read();
        }
    }
}
