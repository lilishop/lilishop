package cn.lili.common.fulu.model;

/**
 * 手机号归属地input dto
 *
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 16:08
 */
public class InputMatchPhoneProductListDto extends CommonRequest {

  public InputMatchPhoneProductListDto() {
    super();
    setMethod("fulu.mobile.info.get");
  }

  /**
   * 面值
   *
   * @return Double
   */
  public Double getFaceValue() {
    Object value = getBizContentValue("face_value");
    return value != null ? Double.valueOf(value.toString()) : null;
  }

  public void setFaceValue(Double faceValue) {
    setBizContent("face_value", faceValue);
  }

  /**
   * 手机号
   *
   * @return String
   */
  public String getPhone() {
    Object value = getBizContentValue("phone");
    return value != null ? String.valueOf(value) : null;
  }

  public void setPhone(String phone) {
    setBizContent("phone", phone);
  }

}
