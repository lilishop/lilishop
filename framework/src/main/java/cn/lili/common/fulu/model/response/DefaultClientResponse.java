package cn.lili.common.fulu.model.response;

/**
 * 响应对象
 *
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 15:06
 */
public class DefaultClientResponse {
  private String code;
  private String message;
  private String result;
  private String sign;

  public String getCode() {
    return code;
  }

  public void setCode(String code) {
    this.code = code;
  }

  public String getMessage() {
    return message;
  }

  public void setMessage(String message) {
    this.message = message;
  }

  public String getResult() {
    return result;
  }

  public void setResult(String result) {
    this.result = result;
  }

  public String getSign() {
    return sign;
  }

  public void setSign(String sign) {
    this.sign = sign;
  }
}
