package cn.lili.common.fulu.model;

/**
 * 订单查单input dto
 *
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 15:23
 */
public class InputOrderGetDto extends CommonRequest {

  public InputOrderGetDto() {
    super();
    setMethod("fulu.order.info.get");
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
