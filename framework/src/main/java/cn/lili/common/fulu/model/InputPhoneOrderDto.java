package cn.lili.common.fulu.model;

/**
 * 话费订单input dto
 *
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 15:31
 */
public class InputPhoneOrderDto extends CommonRequest {

  public InputPhoneOrderDto() {
    super();
    setMethod("fulu.order.mobile.add");
  }

  /**
   * 充值手机号
   *
   * @return String
   */
  public String getChargePhone() {
    Object value = getBizContentValue("charge_phone");
    return value != null ? String.valueOf(value) : null;
  }

  public void setChargePhone(String chargePhone) {
    setBizContent("charge_phone", chargePhone);
  }

  /**
   * 充值数额
   *
   * @return Double
   */
  public Double getChargeValue() {
    Object value = getBizContentValue("charge_value");
    return value != null ? Double.valueOf(value.toString()) : null;
  }

  public void setChargeValue(Double chargeValue) {
    setBizContent("charge_value", chargeValue);
  }

  /**
   * 外部订单号
   *
   * @return String
   */
  public String getCustomerOrderNo() {
    Object value = getBizContentValue("customer_order_no");
    return value != null ? String.valueOf(value) : null;
  }

  public void setCustomerOrderNo(String customerOrderNo) {
    setBizContent("customer_order_no", customerOrderNo);
  }
}
