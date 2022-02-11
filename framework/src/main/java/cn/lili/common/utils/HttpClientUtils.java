package cn.lili.common.utils;

import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.NoHttpResponseException;
import org.apache.http.client.HttpRequestRetryHandler;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.protocol.HttpClientContext;
import org.apache.http.client.utils.URIBuilder;
import org.apache.http.config.Registry;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.conn.routing.HttpRoute;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.socket.LayeredConnectionSocketFactory;
import org.apache.http.conn.socket.PlainConnectionSocketFactory;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager;
import org.apache.http.util.EntityUtils;

import javax.net.ssl.SSLException;
import javax.net.ssl.SSLHandshakeException;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.net.URI;
import java.net.UnknownHostException;
import java.util.Map;

/**
 * HttpClientUtils
 *
 * @author Bulbasaur
 * @since 2021/7/9 1:40 上午
 */
@Slf4j
public class HttpClientUtils {

    /**
     * org.apache.http.impl.client.CloseableHttpClient
     */
    private static CloseableHttpClient httpClient = null;

    //这里就直接默认固定了,因为以下三个参数在新建的method中仍然可以重新配置并被覆盖.
    /**
     * ms毫秒,从池中获取链接超时时间
     */
    static final int CONNECTION_REQUEST_TIMEOUT = 30000;
    /**
     * ms毫秒,建立链接超时时间
     */
    static final int CONNECT_TIMEOUT = 60000;
    /**
     * ms毫秒,读取超时时间
     */
    static final int SOCKET_TIMEOUT = 60000;

    /**
     * 总配置,主要涉及是以下两个参数,如果要作调整没有用到properties会比较后麻烦,但鉴于一经粘贴,随处可用的特点,就不再做依赖性配置化处理了.
     * 而且这个参数同一家公司基本不会变动.
     * 最大总并发,很重要的参数
     */
    static final int MAX_TOTAL = 500;
    /**
     * 每路并发,很重要的参数
     */
    static final int MAX_PER_ROUTE = 100;

    /**
     * 正常情况这里应该配成MAP或LIST
     * 细化配置参数,用来对每路参数做精细化处理,可以管控各ip的流量,比如默认配置请求baidu:80端口最大100个并发链接,
     * 每个细化配置之ip(不重要,在特殊场景很有用)
     */
    static final String DETAIL_HOST_NAME = "http://www.baidu.com";

    /**
     * 每个细化配置之port(不重要,在特殊场景很有用)
     */
    static final int DETAIL_PORT = 80;
    /**
     * 每个细化配置之最大并发数(不重要,在特殊场景很有用)
     */
    static final int DETAIL_MAX_PER_ROUTE = 100;

    private synchronized static CloseableHttpClient getHttpClient() {
        if (null == httpClient) {
            httpClient = init();
        }
        return httpClient;
    }

    /**
     * 链接池初始化 这里最重要的一点理解就是. 让CloseableHttpClient 一直活在池的世界里, 但是HttpPost却一直用完就消掉.
     * 这样可以让链接一直保持着.
     */
    private static CloseableHttpClient init() {
        CloseableHttpClient newHotpoint;

        //设置连接池
        ConnectionSocketFactory plainsf = PlainConnectionSocketFactory.getSocketFactory();
        LayeredConnectionSocketFactory sslsf = SSLConnectionSocketFactory.getSocketFactory();
        Registry<ConnectionSocketFactory> registry = RegistryBuilder.<ConnectionSocketFactory>create().register("http", plainsf).register("https", sslsf).build();
        PoolingHttpClientConnectionManager cm = new PoolingHttpClientConnectionManager(registry);
        //将最大连接数增加
        cm.setMaxTotal(MAX_TOTAL);
        //将每个路由基础的连接增加
        cm.setDefaultMaxPerRoute(MAX_PER_ROUTE);

        //细化配置开始,其实这里用Map或List的for循环来配置每个链接,在特殊场景很有用.
        //将每个路由基础的连接做特殊化配置,一般用不着
        HttpHost httpHost = new HttpHost(DETAIL_HOST_NAME, DETAIL_PORT);
        //将目标主机的最大连接数增加
        cm.setMaxPerRoute(new HttpRoute(httpHost), DETAIL_MAX_PER_ROUTE);
        //细化配置结束

        //请求重试处理
        HttpRequestRetryHandler httpRequestRetryHandler = (exception, executionCount, context) -> {
            if (executionCount >= 2) {//如果已经重试了2次，就放弃
                return false;
            }
            if (exception instanceof NoHttpResponseException) {//如果服务器丢掉了连接，那么就重试
                return true;
            }
            if (exception instanceof SSLHandshakeException) {//不要重试SSL握手异常
                return false;
            }
            if (exception instanceof InterruptedIOException) {//超时
                return false;
            }
            if (exception instanceof UnknownHostException) {//目标服务器不可达
                return false;
            }
            if (exception instanceof SSLException) {//SSL握手异常
                return false;
            }

            HttpClientContext clientContext = HttpClientContext.adapt(context);
            HttpRequest request = clientContext.getRequest();
            //如果请求是幂等的，就再次尝试
            return !(request instanceof HttpEntityEnclosingRequest);
        };

        //配置请求的超时设置
        RequestConfig requestConfig = RequestConfig.custom().setConnectionRequestTimeout(CONNECTION_REQUEST_TIMEOUT).setConnectTimeout(CONNECT_TIMEOUT).setSocketTimeout(SOCKET_TIMEOUT).build();
        newHotpoint = HttpClients.custom().setConnectionManager(cm).setDefaultRequestConfig(requestConfig).setRetryHandler(httpRequestRetryHandler).build();
        return newHotpoint;
    }

    public static String doGet(String url, Map<String, String> param) {

        //httpClient
        CloseableHttpClient httpClient = getHttpClient();

        String resultString = "";
        CloseableHttpResponse response = null;
        try {
            //创建uri
            URIBuilder builder = new URIBuilder(url);
            if (param != null) {
                for (String key : param.keySet()) {
                    builder.addParameter(key, param.get(key));
                }
            }
            URI uri = builder.build();

            //创建http GET请求
            HttpGet httpGet = new HttpGet(uri);

            //执行请求
            response = httpClient.execute(httpGet);
            //判断返回状态是否为200
            if (response.getStatusLine().getStatusCode() == 200) {
                resultString = EntityUtils.toString(response.getEntity(), "UTF-8");
            }
        } catch (Exception e) {
            log.error("get请求错误", e);
        } finally {
            try {
                if (response != null) {
                    response.close();
                }
                httpClient.close();
            } catch (IOException e) {
                log.error("Get错误", e);
            }
        }
        return resultString;
    }
}