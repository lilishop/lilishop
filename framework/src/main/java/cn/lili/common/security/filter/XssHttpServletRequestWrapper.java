package cn.lili.common.security.filter;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import jakarta.servlet.ReadListener;
import jakarta.servlet.ServletInputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletRequestWrapper;
import lombok.extern.slf4j.Slf4j;
import org.owasp.html.HtmlPolicyBuilder;
import org.owasp.html.PolicyFactory;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * 防止Xss
 *
 * @author Chopper
 * @version v1.0
 * 2021-06-04 10:39
 */
@Slf4j
public class XssHttpServletRequestWrapper extends HttpServletRequestWrapper {

    //允许的标签
    private static final String[] allowedTags = {"h1", "h2", "h3", "h4", "h5", "h6",
            "span", "strong",
            "img", "video", "source", "iframe", "code",
            "blockquote", "p", "div",
            "ul", "ol", "li",
            "table", "thead", "caption", "tbody", "tr", "th", "td", "br",
            "a"
    };

    //需要转化的标签
    private static final String[] needTransformTags = {"article", "aside", "command", "datalist", "details", "figcaption", "figure",
            "footer", "header", "hgroup", "section", "summary"};

    //带有超链接的标签
    private static final String[] linkTags = {"img", "video", "source", "a", "iframe", "p"};

    //带有超链接的标签
    private static final String[] allowAttributes = {"style", "src", "href", "target", "width", "height"};

    /**
     * OWASP HTML 安全清洗策略（不可变且线程安全，作为静态常量全局单例复用，避免每次请求重复构建）
     */
    private static final PolicyFactory POLICY = new HtmlPolicyBuilder()
            .allowStandardUrlProtocols()
            // 所有允许的标签
            .allowElements(allowedTags)
            // 内容标签转化为div
            .allowElements((elementName, attributes) -> "div", needTransformTags)
            .allowAttributes(allowAttributes).onElements(linkTags)
            .allowStyling()
            .toFactory();

    public XssHttpServletRequestWrapper(HttpServletRequest request) {
        super(request);
    }

    /**
     * 对数组参数进行特殊字符过滤
     */
    @Override
    public String[] getParameterValues(String name) {
        String[] values = super.getParameterValues(name);
        if (values == null) {
            return new String[0];
        }
        int count = values.length;
        String[] encodedValues = new String[count];
        for (int i = 0; i < count; i++) {
            encodedValues[i] = filterXss(name, values[i]);
        }
        return encodedValues;
    }

    /**
     * 对参数中特殊字符进行过滤
     */
    @Override
    public String getParameter(String name) {
        String description = super.getParameter(name);
        if (description == null) {
            return null;
        }
        return filterXss(name, description);
    }

    /**
     * 获取attribute,特殊字符过滤
     */
    @Override
    public Object getAttribute(String name) {
        Object description = super.getAttribute(name);
        if (description instanceof String) {
            description = filterXss(name, (String) description);
        }
        return description;
    }

    /**
     * 对请求头部进行特殊字符过滤
     */
    @Override
    public String getHeader(String name) {
        String description = super.getHeader(name);
        if (description == null) {
            return null;
        }
        return filterXss(name, description);
    }

    @Override
    public Map<String, String[]> getParameterMap() {
        Map<String, String[]> parameterMap = super.getParameterMap();
        //因为super.getParameterMap()返回的是Map,所以我们需要定义Map的实现类对数据进行封装
        Map<String, String[]> params = new LinkedHashMap<>();
        //如果参数不为空
        if (parameterMap != null) {
            //对map进行遍历
            for (Map.Entry<String, String[]> entry : parameterMap.entrySet()) {
                //根据key获取value
                String[] values = entry.getValue();
                //遍历数组
                for (int i = 0; i < values.length; i++) {
                    String description = values[i];
                    description = filterXss(entry.getKey(), description);
                    //将转义后的数据放回数组中
                    values[i] = description;
                }

                //将转义后的数组put到linkMap当中
                params.put(entry.getKey(), values);
            }
        }
        return params;
    }

    /**
     * 获取输入流
     *
     * @return 过滤后的输入流
     * @throws IOException 异常信息
     */
    @Override
    public ServletInputStream getInputStream() throws IOException {

        BufferedReader bufferedReader = null;
        InputStreamReader reader = null;

        //获取输入流
        ServletInputStream in = null;
        try {
            in = super.getInputStream();
            //用于存储输入流
            StringBuilder body = new StringBuilder();
            reader = new InputStreamReader(in, StandardCharsets.UTF_8);
            bufferedReader = new BufferedReader(reader);
            //按行读取输入流
            String line = bufferedReader.readLine();
            while (line != null) {
                //将获取到的第一行数据append到StringBuffer中
                body.append(line);
                //继续读取下一行流，直到line为空
                line = bufferedReader.readLine();
            }

            // 兼容替换：不再使用过时的 JSONUtil.isJsonObj(String)，改为尝试解析并捕获异常
            if (CharSequenceUtil.isNotEmpty(body)) {
                Map<String, Object> map = null;
                try {
                    map = JSONUtil.parseObj(body.toString());
                } catch (Exception ignore) {
                    map = null;
                }
                if (map != null) {
                    //创建空的map用于存储结果
                    Map<String, Object> resultMap = new HashMap<>(map.size());
                    //遍历数组
                    for (Map.Entry<String, Object> entry : map.entrySet()) {
                        //如果map.get(key)获取到的是字符串就需要进行处理，如果不是直接存储resultMap
                        if (map.get(entry.getKey()) instanceof String) {
                            resultMap.put(entry.getKey(), filterXss(entry.getKey(), entry.getValue().toString()));
                        } else {
                            resultMap.put(entry.getKey(), entry.getValue());
                        }
                    }

                    //将resultMap转换为json字符串
                    String resultStr = JSONUtil.toJsonStr(resultMap);
                    //将json字符串转换为字节
                    final ByteArrayInputStream resultBIS = new ByteArrayInputStream(resultStr.getBytes(StandardCharsets.UTF_8));

                    //实现接口
                    return new ServletInputStream() {
                        @Override
                        public boolean isFinished() {
                            return false;
                        }

                        @Override
                        public boolean isReady() {
                            return false;
                        }

                        @Override
                        public void setReadListener(ReadListener readListener) {
                        }

                        @Override
                        public int read() {
                            return resultBIS.read();
                        }
                    };
                }
            }

            //将json字符串转换为字节
            final ByteArrayInputStream bis = new ByteArrayInputStream(body.toString().getBytes(StandardCharsets.UTF_8));
            //实现接口
            return new ServletInputStream() {
                @Override
                public boolean isFinished() {
                    return false;
                }

                @Override
                public boolean isReady() {
                    return false;
                }

                @Override
                public void setReadListener(ReadListener readListener) {

                }

                @Override
                public int read() {
                    return bis.read();
                }
            };
        } catch (Exception e) {

            log.error("get request inputStream error", e);
            return null;
        } finally {
            //关闭流
            if (bufferedReader != null) {
                bufferedReader.close();
            }
            if (reader != null) {
                reader.close();
            }
            if (in != null) {
                in.close();
            }
        }

    }

    /**
     * 对输入内容执行 XSS 安全清洗。
     * 直接返回经 OWASP 策略清洗后的结果，禁止二次调用 HtmlUtil.unescape 进行反转义，
     * 避免攻击者通过 HTML 实体编码绕过过滤器并在反转义后还原出恶意可执行脚本。
     *
     * @param value 原始输入字符串
     * @return 清洗后的安全字符串
     */
    private String cleanXSS(String value) {
        if (value != null) {
            return POLICY.sanitize(value);
        }
        return null;
    }

    /**
     * 过滤xss
     *
     * @param name  参数名
     * @param value 参数值
     * @return 参数值
     */
    private String filterXss(String name, String value) {
        return cleanXSS(value);
    }

}
