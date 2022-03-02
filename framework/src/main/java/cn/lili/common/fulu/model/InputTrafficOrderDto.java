package cn.lili.common.fulu.model;

/**
 * 流量订单input dto
 *
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 13:50
 */
public class InputTrafficOrderDto extends CommonRequest {

  public InputTrafficOrderDto() {
    super();
    setMethod("fulu.order.data.add");
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
   * 充值数额（M）
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

  /**
   * 流量性质 1:小时 2:日 3:7天 4:月 5:季度 6:半年 7:年
   *
   * @return Integer
   */
  public Integer getPacketKind() {
    Object value = getBizContentValue("packet_kind");
    return value != null ? Integer.valueOf(value.toString()) : null;
  }

  public void setPacketKind(Integer packetKind) {
    setBizContent("packet_kind", packetKind);
  }

}
