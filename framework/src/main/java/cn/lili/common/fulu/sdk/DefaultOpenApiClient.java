package cn.lili.common.fulu.sdk;

import cn.lili.common.fulu.core.http.FuluClient;
import cn.lili.common.fulu.core.http.IFuluClient;
import cn.lili.common.fulu.core.utils.JSONUtil;
import cn.lili.common.fulu.model.CommonRequest;
import org.apache.commons.codec.digest.DigestUtils;

import java.util.Arrays;
import java.util.concurrent.Future;

/**
 * 默认OpenApi客户请求实现
 *
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 16:45
 */
public class DefaultOpenApiClient implements IDefaultOpenApiClient {
  /**
   * 商户AppKey
   */
  private String appKey;
  /**
   * 业务参数
   */
  private CommonRequest bizContent;
  /**
   * http请求
   */
  private IFuluClient fuluClient;
  /**
   * 方法
   */
  private String method;
  /**
   * 应用秘钥
   */
  private String sysSecret;

  //  public DefaultOpenApiClient(String url, String appKey, String sysSecret) {
  //    this.appKey = appKey;
  //    this.sysSecret = sysSecret;
  //
  //    if (url == null || "".equals(url.trim())) {
  //      this.fuluClient = new FuluClient();
  //    } else {
  //      this.fuluClient = new FuluClient(url);
  //    }
  //  }

  /**
   * 如果url没改变，可以使用这个构造
   *
   * @param appKey    appKey
   * @param sysSecret sysSecret
   */
  //  public DefaultOpenApiClient(String appKey, String sysSecret) {
  //    this.appKey = appKey;
  //    this.fuluClient = new FuluClient();
  //    this.sysSecret = sysSecret;
  //  }

  /**
   * 使用public void setBizContent(String bizContent)此方法，method不能为空，必填
   * 或者使用其它构造，调用fulu.sup.open.api.model.CommonRequest的setMethod也可以
   *
   * @param url       url
   * @param appKey    appKey
   * @param sysSecret sysSecret
   * @param method    method
   */
  public DefaultOpenApiClient(String url, String appKey, String sysSecret, String method) {
    this.appKey = appKey;
    this.sysSecret = sysSecret;
    this.method = method;

    if (url == null || "".equals(url.trim())) {
      this.fuluClient = new FuluClient();
    } else {
      this.fuluClient = new FuluClient(url);
    }
  }


  @Override
  public String excute() {
    doSign();
    return fuluClient.send(JSONUtil.toJSON(this.bizContent));
  }

  @Override
  public Future<String> excuteAsync() {
    doSign();
    return fuluClient.sendAsync(JSONUtil.toJSON(this.bizContent));
  }

  @Override
  public void setBizContent(String bizContent) {
    this.bizContent = new CommonRequest();
    this.bizContent.setBizContent(bizContent);
    this.bizContent.setAppKey(this.appKey);
    this.bizContent.setMethod(this.method);
  }

  @Override
  public void setBizObject(CommonRequest bizModel) {
    this.bizContent = bizModel;
    this.bizContent.setAppKey(this.appKey);

    if (method != null && !"".equals(method)) {
      this.bizContent.setMethod(this.method);
    }
  }

  private void doSign() {
    if (bizContent.getMethod() == null || "".equals(bizContent.getMethod().trim())) {
      throw new RuntimeException("param \"method\"  can not blank!");
    }

    this.bizContent.setSign(null);
    this.bizContent.getBizContent();
    String json = JSONUtil.toJSON(this.bizContent);
    char[] charArray = json.toCharArray();
    Arrays.sort(charArray);
    String signStr = new String(charArray);
    signStr = String.format("%s%s", signStr, sysSecret);
    this.bizContent.setSign(DigestUtils.md5Hex(signStr));
  }
}
