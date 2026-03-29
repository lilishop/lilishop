package cn.lili.common.security.filter;


import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.http.HtmlUtil;
import cn.hutool.json.JSONUtil;
import lombok.extern.slf4j.Slf4j;
import org.owasp.html.HtmlPolicyBuilder;
import org.owasp.html.PolicyFactory;

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
            final ByteArrayInputStream bis = new ByteArrayInputStream(body.toString().getBytes());
            //实现接口

            /*
            这俩方法到底是干嘛的？（Servlet 3.1 的宏伟蓝图）
            在早期的 Java EE（Servlet 3.0 之前）里，读取输入流只有那个最经典的 read() 方法。这种读取是**阻塞式（Blocking I/O）**的，数据没来，线程就死等。

            但是到了 Servlet 3.1，官方为了支持非阻塞 I/O (NIO)，在 ServletInputStream 这个抽象类里强行塞进了三个新方法：

            isFinished()：数据读完了吗？

            isReady()：现在可以无阻塞地读数据了吗？

            setReadListener(...)：注册一个监听器，等有数据了叫我。

            这套机制的设计初衷是非常牛逼的，它是为了让 Tomcat 这种容器能在高并发下更高效地复用线程。

            🤦‍♂️ 2. 为什么大家都无脑 return false？（工程师的极致敷衍）
            理想很丰满，现实极其骨感！大厂的开源作者们为什么在这里如此“敷衍”？

            第一点：Spring MVC 根本不用它！
            咱们平时写的 @RestController + @RequestBody，底层走的是 Spring 的 HttpMessageConverter（比如 Jackson）。而 Spring MVC 默认就是同步阻塞模型！Jackson 在解析 JSON 的时候，它是直接去调那个老掉牙的 read() 方法，直到读出 -1 为止。它从头到尾根本就不会去调用 isFinished() 和 isReady()！

            第二点：完美实现它的成本极高！
            既然你的框架不调，但我继承了 ServletInputStream 这个类，编译器（IDEA）又拿刀架在我脖子上，逼着我必须 Override 这三个抽象方法。
            写开源大佬心想：“既然这玩意儿在同步框架里永远不被调用，那我随便写个 return false 糊弄一下编译器就行了呗！”（其实严谨一点，isFinished 应该写 bis.available() == 0，isReady 应该写 true。但因为反正没人调，所以大家索性全填 false 摆烂了 😂）。

            💣 3. 架构师的 X 光：这种“敷衍”会埋下什么雷？
            老哥，既然是架构推演，大刀必须砍到底！
            这种无脑 return false 的 Filter 代码，在传统的 Spring Boot（Spring WebMVC）里跑得极其欢快，稳如老狗。

            但是！一旦系统架构升级，灾难就会降临！
            如果你把这段代码原封不动地搬到了 Spring WebFlux（全异步响应式编程） 或者启用了 Servlet 3.1 纯异步处理的网关项目里。
            底层框架真的会去调用 isReady()，结果得到一个 false，框架就会认为：“哦，流还没准备好，那我挂起线程等一会。”
            结果：你的请求直接死锁（Hang 住），彻底超时，整个系统当场崩溃！
             */
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
            // 自定义策略 HtmlPolicyBuilder 是目前业界公认最强的白名单 XSS 过滤组件
            PolicyFactory policy = new HtmlPolicyBuilder()
                    .allowStandardUrlProtocols()
                    //所有允许的标签
                    .allowElements(allowedTags)
                    //内容标签转化为div
                    .allowElements((elementName, attributes) -> "div", needTransformTags)
                    .allowAttributes(allowAttributes).onElements(linkTags)
                    .allowStyling()
                    .toFactory();
            // basic prepackaged policies for links, tables, integers, images, styles, blocks
            value = policy.sanitize(value);
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
        return cleanXSS(value);
    }

}
