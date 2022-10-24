package cn.lili.common.fulu.model;

/**
 * 卡密订单input dto
 *
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 15:35
 */
public class InputCardOrderDto extends CommonRequest {
  public InputCardOrderDto() {
    super();
    setMethod("fulu.order.card.add");
  }

  /**
   * 购买数量
   *
   * @return Integer
   */
  public Integer getBuyNum() {
    Object value = getBizContentValue("buy_num");
    return value != null ? Integer.valueOf(value.toString()) : null;
  }

  public void setBuyNum(Integer buyNum) {
    setBizContent("buy_num", buyNum);
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
   * 商品编号
   *
   * @return Integer
   */
  public Integer getProductId() {
    Object value = getBizContentValue("product_id");
    return value != null ? Integer.valueOf(value.toString()) : null;
  }

  public void setProductId(Integer productId) {
    setBizContent("product_id", productId);
  }
}
