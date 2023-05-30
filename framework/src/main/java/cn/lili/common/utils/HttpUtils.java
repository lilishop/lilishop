package cn.lili.common.utils;

import com.alibaba.fastjson.JSONObject;
import com.xkcoding.http.HttpUtil;
import com.xkcoding.http.config.HttpConfig;
import com.xkcoding.http.support.HttpHeader;
import com.xkcoding.http.support.httpclient.HttpClientImpl;
import lombok.extern.slf4j.Slf4j;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Iterator;
import java.util.Map;

/**
 * HTTP 工具类
 * @author liushuai
 */
@Slf4j
public class HttpUtils {

    public HttpUtils(HttpConfig config) {
        HttpUtil.setConfig(config);
        HttpUtil.setHttp(new HttpClientImpl());
    }

    public HttpUtils() {
    }


    /**
     * GET 请求
     *
     * @param url URL
     * @return 结果
     */
    public String get(String url) {
        return HttpUtil.get(url);
    }

    /**
     * GET 请求
     *
     * @param url    URL
     * @param params 参数
     * @param header 请求头
     * @param encode 是否需要 url encode
     * @return 结果
     */
    public String get(String url, Map<String, String> params, HttpHeader header, boolean encode) {
        return HttpUtil.get(url, params, header, encode);
    }

    /**
     * POST 请求
     *
     * @param url URL
     * @return 结果
     */
    public String post(String url) {
        return HttpUtil.post(url);
    }

    /**
     * POST 请求
     *
     * @param url  URL
     * @param data JSON 参数
     * @return 结果
     */
    public String post(String url, String data) {
        return HttpUtil.post(url, data);
    }

    /**
     * POST 请求
     *
     * @param url    URL
     * @param data   JSON 参数
     * @param header 请求头
     * @return 结果
     */
    public String post(String url, String data, HttpHeader header) {
        return HttpUtil.post(url, data, header);
    }

    /**
     * POST 请求
     *
     * @param url    URL
     * @param params form 参数
     * @param encode 是否需要 url encode
     * @return 结果
     */
    public String post(String url, Map<String, String> params, boolean encode) {
        return HttpUtil.post(url, params, encode);
    }

    /**
     * POST 请求
     *
     * @param url    URL
     * @param params form 参数
     * @param header 请求头
     * @param encode 是否需要 url encode
     * @return 结果
     */
    public String post(String url, Map<String, String> params, HttpHeader header, boolean encode) {
        return HttpUtil.post(url, params, header, encode);
    }


    /**
     * 静态方法运行参数
     */
    public static final int HTTP_CONN_TIMEOUT = 100000;
    public static final int HTTP_SOCKET_TIMEOUT = 100000;

    /**
     * POST 静态方法请求
     *
     * @param reqUrl
     * @param parameters
     * @param encoding
     * @param connectTimeout
     * @param readTimeout
     * @return
     */
    public static String doPost(String reqUrl, Map<String, String> parameters, String encoding, int connectTimeout,
                                int readTimeout) {
        HttpURLConnection urlConn = null;
        try {
            urlConn = sendPost(reqUrl, parameters, encoding, connectTimeout, readTimeout);
            String responseContent = getContent(urlConn, encoding);
            return responseContent.trim();
        } finally {
            if (urlConn != null) {
                urlConn.disconnect();

            }
        }
    }

    /**
     * post携带json请求 静态方法
     *
     * @param reqUrl         请求地址
     * @param jsonParameters 参数
     * @return
     */
    public static String doPostWithJson(String reqUrl, Map<String, String> jsonParameters) {

        BufferedReader reader = null;
        try {
            //创建连接
            URL url = new URL(reqUrl);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setDoOutput(true);
            connection.setDoInput(true);
            connection.setUseCaches(false);
            connection.setInstanceFollowRedirects(true);
            //设置请求方式
            connection.setRequestMethod("POST");
            //设置发送数据的格式
            connection.setRequestProperty("Content-Type", "application/json");
            connection.connect();
            //一定要用BufferedReader 来接收响应， 使用字节来接收响应的方法是接收不到内容的
            //utf-8编码
            OutputStreamWriter out = new OutputStreamWriter(connection.getOutputStream(), StandardCharsets.UTF_8);
            out.append(JSONObject.toJSONString(jsonParameters));
            out.flush();
            out.close();
            //读取响应
            reader = new BufferedReader(new InputStreamReader(connection.getInputStream(), StandardCharsets.UTF_8));
            String line;
            String res = "";
            while ((line = reader.readLine()) != null) {
                res += line;
            }
            reader.close();

            return res;
        } catch (IOException e) {
            log.error("post请求错误", e);
        }
        //自定义错误信息
        return "error";

    }

    /**
     * post携带json请求 静态方法
     *
     * @param reqUrl 请求地址
     * @param object 对象
     * @return
     */
    public static String doPostWithJson(String reqUrl, Object object) {

        BufferedReader reader = null;
        try {
            //创建连接
            URL url = new URL(reqUrl);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setDoOutput(true);
            connection.setDoInput(true);
            connection.setUseCaches(false);
            connection.setInstanceFollowRedirects(true);
            //设置请求方式
            connection.setRequestMethod("POST");
            //设置发送数据的格式
            connection.setRequestProperty("Content-Type", "application/json");
            connection.connect();
            //一定要用BufferedReader 来接收响应， 使用字节来接收响应的方法是接收不到内容的
            //utf-8编码
            OutputStreamWriter out = new OutputStreamWriter(connection.getOutputStream(), StandardCharsets.UTF_8);
            out.append(JSONObject.toJSONString(object));
            out.flush();
            out.close();
            //读取响应
            reader = new BufferedReader(new InputStreamReader(connection.getInputStream(), StandardCharsets.UTF_8));
            String line;
            String res = "";
            while ((line = reader.readLine()) != null) {
                res += line;
            }
            reader.close();

            return res;
        } catch (IOException e) {
            log.error("post错误", e);
        }
        //自定义错误信息
        return "error";

    }

    /**
     * 发送post请求
     *
     * @param reqUrl
     * @param parameters
     * @param encoding
     * @param connectTimeout
     * @param readTimeout
     * @return
     */
    private static HttpURLConnection sendPost(String reqUrl,
                                              Map<String, String> parameters, String encoding, int connectTimeout, int readTimeout) {
        HttpURLConnection urlConn = null;
        try {
            String params = generatorParamString(parameters, encoding);
            URL url = new URL(reqUrl);
            urlConn = (HttpURLConnection) url.openConnection();
            urlConn.setRequestMethod("POST");
            //（单位：毫秒）jdk
            urlConn.setConnectTimeout(connectTimeout);
            //（单位：毫秒）jdk 1.5换成这个,读操作超时
            urlConn.setReadTimeout(readTimeout);
            urlConn.setDoOutput(true);
            //String按照字节处理是一个好方法
            byte[] b = params.getBytes(encoding);
            urlConn.getOutputStream().write(b, 0, b.length);
            urlConn.getOutputStream().flush();
            urlConn.getOutputStream().close();
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage(), e);
        }
        return urlConn;
    }

    /**
     * get 请求 静态方法
     *
     * @param urlConn
     * @param encoding
     * @return
     */
    private static String getContent(HttpURLConnection urlConn, String encoding) {
        try {
            String responseContent = null;
            InputStream in = urlConn.getInputStream();
            BufferedReader rd = new BufferedReader(new InputStreamReader(in, encoding));
            String tempLine = rd.readLine();
            StringBuffer tempStr = new StringBuffer();
            String crlf = System.getProperty("line.separator");
            while (tempLine != null) {
                tempStr.append(tempLine);
                tempStr.append(crlf);
                tempLine = rd.readLine();
            }
            responseContent = tempStr.toString();
            rd.close();
            in.close();
            return responseContent;
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage(), e);
        }
    }

    /**
     * get 请求 静态方法
     *
     * @param link
     * @param encoding
     * @return
     */
    public static String doGet(String link, String encoding, int connectTimeout, int readTimeout) {
        HttpURLConnection conn = null;
        try {
            URL url = new URL(link);
            conn = (HttpURLConnection) url.openConnection();
            conn.setRequestMethod("GET");
            conn.setConnectTimeout(connectTimeout);
            conn.setReadTimeout(readTimeout);
            BufferedInputStream in = new BufferedInputStream(
                    conn.getInputStream());
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            byte[] buf = new byte[1024];
            for (int i = 0; (i = in.read(buf)) > 0; ) {
                out.write(buf, 0, i);
            }
            out.flush();
            String s = out.toString(encoding);
            return s;
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage(), e);
        } finally {
            if (conn != null) {
                conn.disconnect();
                conn = null;
            }
        }
    }

    /**
     * 将parameters中数据转换成用"&"链接的http请求参数形式
     *
     * @param parameters
     * @return
     */
    private static String generatorParamString(Map<String, String> parameters, String encoding) {
        StringBuffer params = new StringBuffer();
        if (parameters != null) {
            for (Iterator<String> iter = parameters.keySet().iterator(); iter
                    .hasNext(); ) {
                String name = iter.next();
                String value = parameters.get(name);
                params.append(name + "=");
                try {
                    params.append(URLEncoder.encode(value, encoding));
                } catch (UnsupportedEncodingException e) {
                    throw new RuntimeException(e.getMessage(), e);
                } catch (Exception e) {
                    String message = String.format("'%s'='%s'", name, value);
                    throw new RuntimeException(message, e);
                }
                if (iter.hasNext()) {
                    params.append("&");
                }
            }
        }
        return params.toString();
    }
}
