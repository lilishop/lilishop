package cn.lili.modules.system.utils;

import com.alibaba.fastjson.JSONObject;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicHeader;
import org.apache.http.protocol.HTTP;

import java.io.*;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

/**
 * Http工具
 *
 * @author pikachu
 * @date 2018/3/13
 */
public final class HttpUtils {

    public static final int HTTP_CONN_TIMEOUT = 100000;
    public static final int HTTP_SOCKET_TIMEOUT = 100000;

    public static String doPost(String reqUrl, Map<String, String> parameters) {
        return doPost(reqUrl, parameters, "UTF-8", HTTP_CONN_TIMEOUT, HTTP_SOCKET_TIMEOUT);
    }

    public static String doPost(String reqUrl, Map<String, String> parameters, String encoding) {
        return doPost(reqUrl, parameters, encoding, HTTP_CONN_TIMEOUT, HTTP_SOCKET_TIMEOUT);
    }

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
     * post携带json请求
     *
     * @param reqUrl
     * @param jsonParameters
     * @return
     */
    public static String doPostWithJson(String reqUrl, Map<String, String> jsonParameters) {

        BufferedReader reader = null;
        try {
            URL url = new URL(reqUrl);// 创建连接
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setDoOutput(true);
            connection.setDoInput(true);
            connection.setUseCaches(false);
            connection.setInstanceFollowRedirects(true);
            connection.setRequestMethod("POST"); // 设置请求方式
            // connection.setRequestProperty("Accept", "application/json"); // 设置接收数据的格式
            connection.setRequestProperty("Content-Type", "application/json"); // 设置发送数据的格式
            connection.connect();
            //一定要用BufferedReader 来接收响应， 使用字节来接收响应的方法是接收不到内容的
            OutputStreamWriter out = new OutputStreamWriter(connection.getOutputStream(), StandardCharsets.UTF_8); // utf-8编码
            out.append(JSONObject.toJSONString(jsonParameters));
            out.flush();
            out.close();
            // 读取响应
            reader = new BufferedReader(new InputStreamReader(connection.getInputStream(), StandardCharsets.UTF_8));
            String line;
            String res = "";
            while ((line = reader.readLine()) != null) {
                res += line;
            }
            reader.close();

            return res;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return "error"; // 自定义错误信息

    }

    private static HttpURLConnection sendPost(String reqUrl,
                                              Map<String, String> parameters, String encoding, int connectTimeout, int readTimeout) {
        HttpURLConnection urlConn = null;
        try {
            String params = generatorParamString(parameters, encoding);
            URL url = new URL(reqUrl);
            urlConn = (HttpURLConnection) url.openConnection();
            urlConn.setRequestMethod("POST");
            // urlConn
            // .setRequestProperty(
            // "User-Agent",
            // "Mozilla/5.0 (Windows; U; Windows NT 6.1; zh-CN; rv:1.9.2.3) Gecko/20100401 Firefox/3.6.3");
            // （单位：毫秒）jdk
            urlConn.setConnectTimeout(connectTimeout);
            // （单位：毫秒）jdk 1.5换成这个,读操作超时
            urlConn.setReadTimeout(readTimeout);
            urlConn.setDoOutput(true);
            // String按照字节处理是一个好方法
            byte[] b = params.getBytes(encoding);
            urlConn.getOutputStream().write(b, 0, b.length);
            urlConn.getOutputStream().flush();
            urlConn.getOutputStream().close();
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage(), e);
        }
        return urlConn;
    }

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
            // conn.setRequestProperty("User-Agent", "Mozilla/5.0");
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
     * UTF-8编码
     *
     * @param link
     * @return
     */
    public static String doGet(String link) {
        return doGet(link, "UTF-8", HTTP_CONN_TIMEOUT, HTTP_SOCKET_TIMEOUT);
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

    /**
     * post请求封装 参数为{"a":1,"b":2,"c":3}
     *
     * @param path 接口地址
     * @param Info 参数
     * @return
     * @throws IOException
     */
    public static String postResponse(String path, JSONObject Info) {
        HttpClient client = new DefaultHttpClient();
        HttpPost post = new HttpPost(path);

        post.setHeader("Content-Type", "application/json");
        post.addHeader("Authorization", "Basic YWRtaW46");
        String result = "";

        try {
            StringEntity s = new StringEntity(Info.toString(), "utf-8");
            System.out.println("<-------------------->");
            System.out.println(s);
            System.out.println("<-------------------->");
            s.setContentEncoding(new BasicHeader(HTTP.CONTENT_TYPE, "application/json"));
            post.setEntity(s);
            // 发送请求
            HttpResponse httpResponse = client.execute(post);

            // 获取响应输入流
            InputStream inStream = httpResponse.getEntity().getContent();
            BufferedReader reader = new BufferedReader(new InputStreamReader(inStream, StandardCharsets.UTF_8));
            StringBuilder strber = new StringBuilder();
            String line = null;
            while ((line = reader.readLine()) != null) {
                strber.append(line + "\n");
            }
            inStream.close();

            result = strber.toString();
            System.out.println(result);

            if (httpResponse.getStatusLine().getStatusCode() == HttpStatus.SC_OK) {
                System.out.println("请求服务器成功，做相应处理");
            } else {
                System.out.println("请求服务端失败");
            }

        } catch (Exception e) {
            System.out.println("请求异常");
            throw new RuntimeException(e);
        }

        return result;
    }

    public static String http(String url, String proxyUrl, int proxyPort, Map<String, String> params, String chartSet)
            throws Exception {
        URL u = null;
        HttpURLConnection con = null;
        // 构建请求参数
        StringBuffer sb = new StringBuffer();
        OutputStreamWriter osw = null;
        BufferedReader br = null;
        if (params != null) {
            int i = 0;
            for (Entry<String, String> e : params.entrySet()) {
                if (i != 0) {
                    sb.append("&");
                } else {
                    i++;
                }
                sb.append(e.getKey());
                if (e.getValue() != null && !e.getValue().equals("")) {
                    sb.append("=");
                    sb.append(e.getValue());///URLEncoder.encode(sign, "UTF-8")
                }
            }
        }
        System.out.println("连接:" + url);
        System.out.println("发送:" + sb.toString());
        try {
            u = new URL(url);
            if (null != proxyUrl && !proxyUrl.equals("")) {
                System.out.println("代理的IP是：" + proxyUrl + ",代理端口：" + proxyPort);
                Proxy proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(proxyUrl, proxyPort));
                con = (HttpURLConnection) u.openConnection(proxy);
            } else {
                con = (HttpURLConnection) u.openConnection();
            }
            con.setConnectTimeout(30000);
            con.setReadTimeout(700000);
            con.setRequestMethod("POST");
            con.setDoOutput(true);
            con.setDoInput(true);
            con.setUseCaches(false);
            con.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
            con.setRequestProperty("Charset", "UTF-8");
            osw = new OutputStreamWriter(con.getOutputStream(), StandardCharsets.UTF_8);
            osw.write(sb.toString());
            osw.flush();
        } catch (SocketTimeoutException e) {
            throw new Exception();
        } catch (Exception e) {
            throw new Exception();
        } finally {
            try {
                osw.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        StringBuffer buffer = new StringBuffer();
        try {
            br = new BufferedReader(new InputStreamReader(con.getInputStream(),
                    StandardCharsets.UTF_8));
            String temp;
            while ((temp = br.readLine()) != null) {
                buffer.append(temp);
                buffer.append("\n");
            }
        } catch (SocketTimeoutException e) {
            throw new Exception();
        } catch (FileNotFoundException e) {
            throw new Exception();
        } catch (Exception e) {
            throw new Exception();
        } finally {
            try {
                if (osw != null) {
                    osw.close();
                }
                if (br != null) {
                    br.close();
                }
                if (con != null) {
                    con.disconnect();
                }
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
        return buffer.toString();
    }
}