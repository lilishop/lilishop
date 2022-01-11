package cn.lili.common.security.filter;


import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.http.HtmlUtil;
import cn.hutool.json.JSONUtil;
import lombok.extern.slf4j.Slf4j;
import org.owasp.html.HtmlPolicyBuilder;
import org.owasp.html.PolicyFactory;
import org.owasp.html.Sanitizers;

import javax.servlet.ReadListener;
import javax.servlet.ServletInputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
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


    /**
     * xss过滤参数
     *
     * @todo 这里的参数应该更智能些，例如iv，前端的参数包含这两个字母就会放过，这是有问题的
     */
    private static final String[] IGNORE_FIELD = {
            "logo",
            "url",
            "photo",
            "intro",
            "content",
            "name",
            "image",
            "encrypted",
            "iv",
            "mail",
            "sell",
            "id",
            "price",
            "prop",
            "reply",
            "profile",
            "privateKey",
            "wechatpay",
    };

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
    private static final String[] linkTags = {"img", "video", "source", "a", "iframe"};


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
        String value = super.getParameter(name);
        if (value == null) {
            return null;
        }
        return filterXss(name, value);
    }

    /**
     * 获取attribute,特殊字符过滤
     */
    @Override
    public Object getAttribute(String name) {
        Object value = super.getAttribute(name);
        if (value instanceof String) {
            value = filterXss(name, (String) value);
        }
        return value;
    }

    /**
     * 对请求头部进行特殊字符过滤
     */
    @Override
    public String getHeader(String name) {
        String value = super.getHeader(name);
        if (value == null) {
            return null;
        }
        return filterXss(name, value);
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
                    String value = values[i];
                    value = filterXss(entry.getKey(), value);
                    //将转义后的数据放回数组中
                    values[i] = value;
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
            if (CharSequenceUtil.isNotEmpty(body) && Boolean.TRUE.equals(JSONUtil.isJsonObj(body.toString()))) {
                //将body转换为map
                Map<String, Object> map = JSONUtil.parseObj(body.toString());
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
                final ByteArrayInputStream resultBIS = new ByteArrayInputStream(resultStr.getBytes());

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

            //将json字符串转换为字节
            final ByteArrayInputStream bis = new ByteArrayInputStream(body.toString().getBytes());
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

    private String cleanXSS(String value) {
        if (value != null) {
            // 自定义策略
            PolicyFactory policy = new HtmlPolicyBuilder()
                    .allowStandardUrlProtocols()
                    //所有允许的标签
                    .allowElements(allowedTags)
                    //内容标签转化为div
                    .allowElements((elementName, attributes) -> "div", needTransformTags)
                    .allowAttributes("src", "href", "target", "width", "height").onElements(linkTags)
                    //校验链接中的是否为http
//                    .allowUrlProtocols("https")
                    .toFactory();
            // basic prepackaged policies for links, tables, integers, images, styles, blocks
            value = Sanitizers.FORMATTING
                    .and(Sanitizers.STYLES)
                    .and(Sanitizers.IMAGES)
                    .and(Sanitizers.LINKS)
                    .and(Sanitizers.BLOCKS)
                    .and(Sanitizers.TABLES)
                    .and(policy)
                    .sanitize(value);
        }
        return HtmlUtil.unescape(value);
    }

    /**
     * 过滤xss
     *
     * @param name  参数名
     * @param value 参数值
     * @return 参数值
     */
    private String filterXss(String name, String value) {
//        if (CharSequenceUtil.containsAny(name.toLowerCase(Locale.ROOT), IGNORE_FIELD)) {
//            // 忽略的处理，（过滤敏感字符）
//            return value;
//        } else {
//            return cleanXSS(value);
//        }
        return cleanXSS(value);
    }

}
