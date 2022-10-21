package cn.lili.common.fulu.model;

import cn.lili.common.fulu.core.utils.DateFormatUtil;
import cn.lili.common.fulu.core.utils.JSONUtil;
import com.google.gson.annotations.SerializedName;
import com.google.gson.reflect.TypeToken;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 10:49
 */
public class CommonRequest implements Serializable {
  private static final long serialVersionUID = 2L;

  @SerializedName(value = "app_auth_token")
  private String appAuthToken = "";
  @SerializedName(value = "app_key")
  private String appKey;
  @SerializedName(value = "biz_content")
  private String bizContent = "{}";
  private transient Map<String, Object> bizContentMap = Collections.emptyMap();
  private String charset;
  private String format;
  private String method;
  private String sign;
  @SerializedName(value = "sign_type")
  private String signType;
  private String timestamp;
  private String version;

  public CommonRequest() {
    format = "json";
    version = "2.0";
    charset = "utf-8";
    signType = "md5";
    //    timestamp = "2019-08-20 13:59:38";
    timestamp = DateFormatUtil.currentDateTime();
  }


  public String getAppAuthToken() {
    return appAuthToken;
  }

  public void setAppAuthToken(String appAuthToken) {
    this.appAuthToken = appAuthToken;
  }

  public String getAppKey() {
    return appKey;
  }

  public void setAppKey(String appKey) {
    this.appKey = appKey;
  }

  public String getBizContent() {
    if (!bizContentMap.isEmpty()) {
      bizContent = JSONUtil.toJSON(bizContentMap);
    }
    return bizContent;
  }

  /**
   * 直接将变量以json格式保存
   *
   * @param bizContent bizContent
   */
  public void setBizContent(String bizContent) {
    if (bizContent != null && !"".equals(bizContent.trim())) {
      Map<String, Object> dataMap = JSONUtil.fromJSON(bizContent, new TypeToken<Map<String, Object>>() {
      });

      if (bizContentMap.isEmpty()) {
        bizContentMap = new HashMap<String, Object>();
      }
      bizContentMap.putAll(dataMap);

    }
  }

  public String getCharset() {
    return charset;
  }

  public void setCharset(String charset) {
    this.charset = charset;
  }

  public String getFormat() {
    return format;
  }

  public void setFormat(String format) {
    this.format = format;
  }

  public String getMethod() {
    return method;
  }

  public void setMethod(String method) {
    this.method = method;
  }

  public String getSign() {
    return sign;
  }

  public void setSign(String sign) {
    this.sign = sign;
  }

  public String getSignType() {
    return signType;
  }

  public void setSignType(String signType) {
    this.signType = signType;
  }

  public String getTimestamp() {
    return timestamp;
  }

  public void setTimestamp(String timestamp) {
    this.timestamp = timestamp;
  }

  public String getVersion() {
    return version;
  }

  public void setVersion(String version) {
    this.version = version;
  }

  /**
   * 以key:value形式保存，程序会转换为json格式
   *
   * @param key   key
   * @param value value
   */
  public void setBizContent(String key, Object value) {
    if (bizContentMap.isEmpty()) {
      bizContentMap = new HashMap<String, Object>();
    }
    bizContentMap.put(key, value);
  }

  protected Object getBizContentValue(String key) {
    return bizContentMap.get(key);
  }
}
