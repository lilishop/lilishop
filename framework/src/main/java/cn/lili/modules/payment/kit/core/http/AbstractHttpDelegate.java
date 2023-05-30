package cn.lili.modules.payment.kit.core.http;

import cn.hutool.core.io.FileUtil;
import cn.hutool.core.net.SSLContextBuilder;
import cn.hutool.core.net.SSLProtocols;
import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.http.HttpUtil;
import cn.lili.modules.payment.kit.core.PaymentHttpResponse;

import javax.net.ssl.KeyManager;
import javax.net.ssl.KeyManagerFactory;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.SecureRandom;
import java.util.Map;

/**
 * Http 代理类
 *
 * @author Chopper
 * @since 2021-01-25 15:10
 */
public abstract class AbstractHttpDelegate {

    /**
     * get 请求
     *
     * @param url 请求url
     * @return {@link String} 请求返回的结果
     */
    public String get(String url) {
        return HttpUtil.get(url);
    }

    /**
     * get 请求
     *
     * @param url      请求url
     * @param paramMap 请求参数
     * @return {@link String} 请求返回的结果
     */
    public String get(String url, Map<String, Object> paramMap) {
        return HttpUtil.get(url, paramMap);
    }

    /**
     * get 请求
     *
     * @param url      请求url
     * @param paramMap 请求参数
     * @param headers  请求头
     * @return {@link PaymentHttpResponse} 请求返回的结果
     */
    public PaymentHttpResponse get(String url, Map<String, Object> paramMap, Map<String, String> headers) {
        PaymentHttpResponse response = new PaymentHttpResponse();
        HttpResponse httpResponse = getToResponse(url, paramMap, headers);
        response.setBody(httpResponse.body());
        response.setStatus(httpResponse.getStatus());
        response.setHeaders(httpResponse.headers());
        return response;
    }

    /**
     * post 请求
     *
     * @param url  请求url
     * @param data 请求参数
     * @return {@link String} 请求返回的结果
     */
    public String post(String url, String data) {
        return HttpUtil.post(url, data);
    }

    /**
     * post 请求
     *
     * @param url      请求url
     * @param paramMap 请求参数
     * @return {@link String} 请求返回的结果
     */
    public String post(String url, Map<String, Object> paramMap) {
        return HttpUtil.post(url, paramMap);
    }

    /**
     * post 请求
     *
     * @param url      请求url
     * @param paramMap 请求参数
     * @param headers  请求头
     * @return {@link PaymentHttpResponse}  请求返回的结果
     */
    public PaymentHttpResponse post(String url, Map<String, Object> paramMap, Map<String, String> headers) {
        PaymentHttpResponse response = new PaymentHttpResponse();
        HttpResponse httpResponse = postToResponse(url, headers, paramMap);
        response.setBody(httpResponse.body());
        response.setStatus(httpResponse.getStatus());
        response.setHeaders(httpResponse.headers());
        return response;
    }

    /**
     * post 请求
     *
     * @param url     请求url
     * @param data    请求参数
     * @param headers 请求头
     * @return {@link PaymentHttpResponse}  请求返回的结果
     */
    public PaymentHttpResponse post(String url, String data, Map<String, String> headers) {
        PaymentHttpResponse response = new PaymentHttpResponse();
        HttpResponse httpResponse = postToResponse(url, headers, data);
        response.setBody(httpResponse.body());
        response.setStatus(httpResponse.getStatus());
        response.setHeaders(httpResponse.headers());
        return response;
    }

    /**
     * patch 请求
     *
     * @param url      请求url
     * @param paramMap 请求参数
     * @param headers  请求头
     * @return {@link PaymentHttpResponse}  请求返回的结果
     */
    public PaymentHttpResponse patch(String url, Map<String, Object> paramMap, Map<String, String> headers) {
        PaymentHttpResponse response = new PaymentHttpResponse();
        HttpResponse httpResponse = patchToResponse(url, headers, paramMap);
        response.setBody(httpResponse.body());
        response.setStatus(httpResponse.getStatus());
        response.setHeaders(httpResponse.headers());
        return response;
    }

    /**
     * patch 请求
     *
     * @param url     请求url
     * @param data    请求参数
     * @param headers 请求头
     * @return {@link PaymentHttpResponse}  请求返回的结果
     */
    public PaymentHttpResponse patch(String url, String data, Map<String, String> headers) {
        PaymentHttpResponse response = new PaymentHttpResponse();
        HttpResponse httpResponse = patchToResponse(url, headers, data);
        response.setBody(httpResponse.body());
        response.setStatus(httpResponse.getStatus());
        response.setHeaders(httpResponse.headers());
        return response;
    }

    /**
     * delete 请求
     *
     * @param url      请求url
     * @param paramMap 请求参数
     * @param headers  请求头
     * @return {@link PaymentHttpResponse}  请求返回的结果
     */
    public PaymentHttpResponse delete(String url, Map<String, Object> paramMap, Map<String, String> headers) {
        PaymentHttpResponse response = new PaymentHttpResponse();
        HttpResponse httpResponse = deleteToResponse(url, headers, paramMap);
        response.setBody(httpResponse.body());
        response.setStatus(httpResponse.getStatus());
        response.setHeaders(httpResponse.headers());
        return response;
    }

    /**
     * delete 请求
     *
     * @param url     请求url
     * @param data    请求参数
     * @param headers 请求头
     * @return {@link PaymentHttpResponse}  请求返回的结果
     */
    public PaymentHttpResponse delete(String url, String data, Map<String, String> headers) {
        PaymentHttpResponse response = new PaymentHttpResponse();
        HttpResponse httpResponse = deleteToResponse(url, headers, data);
        response.setBody(httpResponse.body());
        response.setStatus(httpResponse.getStatus());
        response.setHeaders(httpResponse.headers());
        return response;
    }

    /**
     * put 请求
     *
     * @param url      请求url
     * @param paramMap 请求参数
     * @param headers  请求头
     * @return {@link PaymentHttpResponse}  请求返回的结果
     */
    public PaymentHttpResponse put(String url, Map<String, Object> paramMap, Map<String, String> headers) {
        PaymentHttpResponse response = new PaymentHttpResponse();
        HttpResponse httpResponse = putToResponse(url, headers, paramMap);
        response.setBody(httpResponse.body());
        response.setStatus(httpResponse.getStatus());
        response.setHeaders(httpResponse.headers());
        return response;
    }

    /**
     * put 请求
     *
     * @param url     请求url
     * @param data    请求参数
     * @param headers 请求头
     * @return {@link PaymentHttpResponse}  请求返回的结果
     */
    public PaymentHttpResponse put(String url, String data, Map<String, String> headers) {
        PaymentHttpResponse response = new PaymentHttpResponse();
        HttpResponse httpResponse = putToResponse(url, headers, data);
        response.setBody(httpResponse.body());
        response.setStatus(httpResponse.getStatus());
        response.setHeaders(httpResponse.headers());
        return response;
    }

    /**
     * 上传文件
     *
     * @param url      请求url
     * @param data     请求参数
     * @param certPath 证书路径
     * @param certPass 证书密码
     * @param filePath 上传文件路径
     * @param protocol 协议
     * @return {@link String}  请求返回的结果
     */
    public String upload(String url, String data, String certPath, String certPass, String filePath, String protocol) {
        try {
            File file = FileUtil.newFile(filePath);
            return HttpRequest.post(url)
                    .setSSLSocketFactory(SSLContextBuilder
                            .create()
                            .setProtocol(protocol)
                            .setKeyManagers(getKeyManager(certPass, certPath, null))
                            .setSecureRandom(new SecureRandom())
                            .build().getSocketFactory()
                    )
                    .header("Content-Type", "multipart/form-data;boundary=\"boundary\"")
                    .form("file", file)
                    .form("meta", data)
                    .execute()
                    .body();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 上传文件
     *
     * @param url      请求url
     * @param data     请求参数
     * @param certPath 证书路径
     * @param certPass 证书密码
     * @param filePath 上传文件路径
     * @return {@link String}  请求返回的结果
     */
    public String upload(String url, String data, String certPath, String certPass, String filePath) {
        return upload(url, data, certPath, certPass, filePath, SSLProtocols.TLSv1);
    }

    /**
     * post 请求
     *
     * @param url      请求url
     * @param data     请求参数
     * @param certPath 证书路径
     * @param certPass 证书密码
     * @param protocol 协议
     * @return {@link String} 请求返回的结果
     */
    public String post(String url, String data, String certPath, String certPass, String protocol) {
        try {
            return HttpRequest.post(url)
                    .setSSLSocketFactory(SSLContextBuilder
                            .create()
                            .setProtocol(protocol)
                            .setKeyManagers(getKeyManager(certPass, certPath, null))
                            .setSecureRandom(new SecureRandom())
                            .build().getSocketFactory()
                    )
                    .body(data)
                    .execute()
                    .body();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * post 请求
     *
     * @param url      请求url
     * @param data     请求参数
     * @param certPath 证书路径
     * @param certPass 证书密码
     * @return {@link String} 请求返回的结果
     */
    public String post(String url, String data, String certPath, String certPass) {
        return post(url, data, certPath, certPass, SSLProtocols.TLSv1);
    }

    /**
     * post 请求
     *
     * @param url      请求url
     * @param data     请求参数
     * @param certFile 证书文件输入流
     * @param certPass 证书密码
     * @param protocol 协议
     * @return {@link String} 请求返回的结果
     */
    public String post(String url, String data, InputStream certFile, String certPass, String protocol) {
        try {
            return HttpRequest.post(url)
                    .setSSLSocketFactory(SSLContextBuilder
                            .create()
                            .setProtocol(protocol)
                            .setKeyManagers(getKeyManager(certPass, null, certFile))
                            .setSecureRandom(new SecureRandom())
                            .build().getSocketFactory()
                    )
                    .body(data)
                    .execute()
                    .body();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * post 请求
     *
     * @param url      请求url
     * @param data     请求参数
     * @param certFile 证书文件输入流
     * @param certPass 证书密码
     * @return {@link String} 请求返回的结果
     */
    public String post(String url, String data, InputStream certFile, String certPass) {
        return post(url, data, certFile, certPass, SSLProtocols.TLSv1);
    }

    /**
     * get 请求
     *
     * @param url      请求url
     * @param paramMap 请求参数
     * @param headers  请求头
     * @return {@link HttpResponse} 请求返回的结果
     */
    private HttpResponse getToResponse(String url, Map<String, Object> paramMap, Map<String, String> headers) {
        return HttpRequest.get(url)
                .addHeaders(headers)
                .form(paramMap)
                .execute();
    }

    /**
     * post 请求
     *
     * @param url     请求url
     * @param headers 请求头
     * @param data    请求参数
     * @return {@link HttpResponse} 请求返回的结果
     */
    private HttpResponse postToResponse(String url, Map<String, String> headers, String data) {
        return HttpRequest.post(url)
                .addHeaders(headers)
                .body(data)
                .execute();
    }

    /**
     * post 请求
     *
     * @param url      请求url
     * @param headers  请求头
     * @param paramMap 请求参数
     * @return {@link HttpResponse} 请求返回的结果
     */
    private HttpResponse postToResponse(String url, Map<String, String> headers, Map<String, Object> paramMap) {
        return HttpRequest.post(url)
                .addHeaders(headers)
                .form(paramMap)
                .execute();
    }

    /**
     * patch 请求
     *
     * @param url      请求url
     * @param headers  请求头
     * @param paramMap 请求参数
     * @return {@link HttpResponse} 请求返回的结果
     */
    private HttpResponse patchToResponse(String url, Map<String, String> headers, Map<String, Object> paramMap) {
        return HttpRequest.patch(url)
                .addHeaders(headers)
                .form(paramMap)
                .execute();
    }

    /**
     * patch 请求
     *
     * @param url     请求url
     * @param headers 请求头
     * @param data    请求参数
     * @return {@link HttpResponse} 请求返回的结果
     */
    private HttpResponse patchToResponse(String url, Map<String, String> headers, String data) {
        return HttpRequest.patch(url)
                .addHeaders(headers)
                .body(data)
                .execute();
    }

    /**
     * delete 请求
     *
     * @param url     请求url
     * @param headers 请求头
     * @param data    请求参数
     * @return {@link HttpResponse} 请求返回的结果
     */
    private HttpResponse deleteToResponse(String url, Map<String, String> headers, String data) {
        return HttpRequest.delete(url)
                .addHeaders(headers)
                .body(data)
                .execute();
    }

    /**
     * delete 请求
     *
     * @param url      请求url
     * @param headers  请求头
     * @param paramMap 请求参数
     * @return {@link HttpResponse} 请求返回的结果
     */
    private HttpResponse deleteToResponse(String url, Map<String, String> headers, Map<String, Object> paramMap) {
        return HttpRequest.delete(url)
                .addHeaders(headers)
                .form(paramMap)
                .execute();
    }

    /**
     * put 请求
     *
     * @param url     请求url
     * @param headers 请求头
     * @param data    请求参数
     * @return {@link HttpResponse} 请求返回的结果
     */
    private HttpResponse putToResponse(String url, Map<String, String> headers, String data) {
        return HttpRequest.put(url)
                .addHeaders(headers)
                .body(data)
                .execute();
    }

    /**
     * put 请求
     *
     * @param url      请求url
     * @param headers  请求头
     * @param paramMap 请求参数
     * @return {@link HttpResponse} 请求返回的结果
     */
    private HttpResponse putToResponse(String url, Map<String, String> headers, Map<String, Object> paramMap) {
        return HttpRequest.put(url)
                .addHeaders(headers)
                .form(paramMap)
                .execute();
    }


    private KeyManager[] getKeyManager(String certPass, String certPath, InputStream certFile) throws Exception {
        KeyStore clientStore = KeyStore.getInstance("PKCS12");
        if (certFile != null) {
            clientStore.load(certFile, certPass.toCharArray());
        } else {
            clientStore.load(new FileInputStream(certPath), certPass.toCharArray());
        }
        KeyManagerFactory kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
        kmf.init(clientStore, certPass.toCharArray());
        return kmf.getKeyManagers();
    }
}
