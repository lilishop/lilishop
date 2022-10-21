package cn.lili.common.fulu.core.utils;


import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.protocol.HTTP;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.*;
import java.util.*;

/**
 * @author cheny
 */
public class HttpUtil {
  private static Logger log = LoggerFactory.getLogger(HttpUtil.class);

  public final static int CONNECT_TIMEOUT = 10000;
  public final static int READ_TIMEOUT = 10000;

  private static final String ENCODING_GBK = "GBK";

  /**
   * POST请求，json字符串形式数据
   *
   * @param url   请求地址
   * @param param 请求的json数据
   * @return response body
   * @throws Exception
   */
  public static String sendPostJson(String url, String param) {
    return sendPostWithHeads(url, param, "application/json", null);
  }

  private static String sendPostWithHeads(String url, String param, String contentType, Map<String, String> heads) {
    PrintWriter out = null;
    BufferedReader in = null;
    StringBuilder result = new StringBuilder();
    HttpURLConnection conn = null;

    try {
      URL realUrl = new URL(url);
      // 打开和URL之间的连接
      conn = (HttpURLConnection) realUrl.openConnection();
      // 设置通用的请求属性
      conn.setRequestMethod("POST");// 提交模式
      conn.setConnectTimeout(CONNECT_TIMEOUT);// 连接超时 单位毫秒
      conn.setReadTimeout(READ_TIMEOUT);// 读取超时 单位毫秒
      if (contentType != null && !contentType.isEmpty()) {
        conn.setRequestProperty("Content-Type", contentType);
      }
      conn.setRequestProperty("accept", "*/*");
      conn.setRequestProperty("connection", "Keep-Alive");
      conn.setRequestProperty("user-agent",
          "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/65.0.3325.181 Safari/537.36");

      // 添加头信息
      if (heads != null && !heads.isEmpty()) {
        for (String key : heads.keySet()) {
          conn.setRequestProperty(key, heads.get(key));
        }
      }

      // 发送POST请求必须设置如下两行
      conn.setDoOutput(true);
      conn.setDoInput(true);
      // 获取URLConnection对象对应的输出流
      out = new PrintWriter(conn.getOutputStream());
      // 发送请求参数
      out.print(param);
      // flush输出流的缓冲
      out.flush();
      // 定义BufferedReader输入流来读取URL的响应
      in = new BufferedReader(new InputStreamReader(conn.getInputStream(), "UTF-8"));
      String line;
      while ((line = in.readLine()) != null) {
        result.append(line);
      }
    } catch (Exception e) {
      e.printStackTrace();
      throw new RuntimeException("send  POST  request exception :" + e.getMessage(), e);
    } finally {
      try {
        if (out != null) {
          out.close();
        }
        if (in != null) {
          in.close();
        }
        if (conn != null) {
          conn.disconnect();
        }
      } catch (Exception e2) {
        throw new RuntimeException("close Connection exception :" + e2.getMessage(), e2);
      }
    }
    return result.toString();
  }

  /**
   * POST请求，String字符串形式数据
   *
   * @param url
   * @param paramXmlStr
   * @return
   */
  public static String sendPostXmlStr(String url, String paramXmlStr) {
    String tmpparamXmlStr = "";
    try {
      tmpparamXmlStr = URLEncoder.encode(paramXmlStr, ENCODING_GBK);
      tmpparamXmlStr = sendPostWithHeads(url, tmpparamXmlStr, "application/xml", null);
      tmpparamXmlStr = URLDecoder.decode(tmpparamXmlStr,ENCODING_GBK);

    }catch (Exception e){
      e.printStackTrace();
      log.error("post请求URL数据转码报错，{}", e.getMessage());
    }
    return tmpparamXmlStr;
  }



  /**
   * POST请求，String字符串形式数据
   *
   * @param url
   * @param paramXmlStr
   * @return
   */
  public static String sendPostXmlUrlencode(String url, String paramXmlStr) {
    String tmpparamXmlStr = "";
    Map<String, String> rspMap = new LinkedHashMap<>();
    rspMap.put("req", paramXmlStr);

    try {
      tmpparamXmlStr = URLEncoder.encode(paramXmlStr, ENCODING_GBK);
      tmpparamXmlStr = doPostMapParams(url, rspMap);
      tmpparamXmlStr = URLDecoder.decode(tmpparamXmlStr,ENCODING_GBK);
    }catch (Exception e){
      e.printStackTrace();
      log.error("post请求URL数据转码报错，{}", e.getMessage());
    }
    return tmpparamXmlStr;
  }

  /**
   * 发送POST请求
   * @param url String对象为 目的地址
   * @param parameters  请求参数，Map类型。
   * @return 远程响应结果
   */
  public static String doPostMapParams(String url, Map<String, String> parameters) {

    BufferedReader in = null;
    try {
      // 定义HttpClient
      HttpClient client = new DefaultHttpClient();
      // 实例化HTTP方法
      HttpPost request = new HttpPost();
      request.setURI(new URI(url));

      //设置参数
      List<NameValuePair> nvps = new ArrayList<NameValuePair>();
      for (Iterator iter = parameters.keySet().iterator(); iter.hasNext();) {
        String name = (String) iter.next();
        String value = String.valueOf(parameters.get(name));
        nvps.add(new BasicNameValuePair(name, value));
      }
      request.setEntity(new UrlEncodedFormEntity(nvps, HTTP.UTF_8));

      HttpResponse response = client.execute(request);
      int code = response.getStatusLine().getStatusCode();
      if(code == 200){	//请求成功
        in = new BufferedReader(new InputStreamReader(response.getEntity()
                .getContent(),"UTF-8"));
        StringBuffer sb = new StringBuffer("");
        String line = "";
        String NL = System.getProperty("line.separator");
        while ((line = in.readLine()) != null) {
          sb.append(line + NL);
        }

        in.close();
        return sb.toString();
      }else{
        System.out.println("状态码：" + code);
        return null;
      }
    }
    catch(Exception e) {
      e.printStackTrace();
      return null;
    }
  }


}
