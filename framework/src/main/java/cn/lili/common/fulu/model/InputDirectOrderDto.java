package cn.lili.common.fulu.model;

/**
 * 直充商品订单input dto
 *
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 15:39
 */
public class InputDirectOrderDto extends CommonRequest {

  public InputDirectOrderDto() {
    super();
    setMethod("fulu.order.direct.add");
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
   * 充值账号
   *
   * @return String
   */
  public String getChargeAccount() {
    Object value = getBizContentValue("charge_account");
    return value != null ? String.valueOf(value) : null;
  }

  public void setChargeAccount(String chargeAccount) {
    setBizContent("charge_account", chargeAccount);
  }

  /**
   * 充值游戏名称
   *
   * @return String
   */
  public String getChargeGameName() {
    Object value = getBizContentValue("charge_game_name");
    return value != null ? String.valueOf(value) : null;
  }

  public void setChargeGameName(String chargeGameName) {
    setBizContent("charge_game_name", chargeGameName);
  }

  /**
   * 充值游戏区
   *
   * @return String
   */
  public String getChargeGameRegion() {
    Object value = getBizContentValue("charge_game_region");
    return value != null ? String.valueOf(value) : null;
  }

  public void setChargeGameRegion(String chargeGameRegion) {
    setBizContent("charge_game_region", chargeGameRegion);
  }

  /**
   * 充值游戏角色
   *
   * @return String
   */
  public String getChargeGameRole() {
    Object value = getBizContentValue("charge_game_role");
    return value != null ? String.valueOf(value) : null;
  }

  public void setChargeGameRole(String chargeGameRole) {
    setBizContent("charge_game_role", chargeGameRole);
  }

  /**
   * 充值游戏服
   *
   * @return String
   */
  public String getChargeGameSrv() {
    Object value = getBizContentValue("charge_game_srv");
    return value != null ? String.valueOf(value) : null;
  }

  public void setChargeGameSrv(String chargeGameSrv) {
    setBizContent("charge_game_srv", chargeGameSrv);
  }

  /**
   * 下单真实Ip，区域商品要传
   *
   * @return String
   */
  public String getChargeIp() {
    Object value = getBizContentValue("charge_ip");
    return value != null ? String.valueOf(value) : null;
  }

  public void setChargeIp(String chargeIp) {
    setBizContent("charge_ip", chargeIp);
  }

  /**
   * 充值密码，部分游戏类要传
   *
   * @return String
   */
  public String getChargePassword() {
    Object value = getBizContentValue("charge_password");
    return value != null ? String.valueOf(value) : null;
  }

  public void setChargePassword(String chargePassword) {
    setBizContent("charge_password", chargePassword);
  }

  /**
   * 充值类型
   *
   * @return String
   */
  public String getChargeType() {
    Object value = getBizContentValue("charge_type");
    return value != null ? String.valueOf(value) : null;
  }

  public void setChargeType(String chargeType) {
    setBizContent("charge_type", chargeType);
  }

  /**
   * 联系QQ
   *
   * @return String
   */
  public String getContactQq() {
    Object value = getBizContentValue("contact_qq");
    return value != null ? String.valueOf(value) : null;
  }

  public void setContactQq(String contactQq) {
    setBizContent("contact_qq", contactQq);
  }

  /**
   * 联系电话
   *
   * @return String
   */
  public String getContactTel() {
    Object value = getBizContentValue("contact_tel");
    return value != null ? String.valueOf(value) : null;
  }

  public void setContactTel(String contactTel) {
    setBizContent("contact_tel", contactTel);
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

  /**
   * 剩余数量
   *
   * @return Integer
   */
  public Integer getRemainingNumber() {
    Object value = getBizContentValue("remaining_number");
    return value != null ? Integer.valueOf(value.toString()) : null;
  }

  public void setRemainingNumber(Integer remainingNumber) {
    setBizContent("remaining_number", remainingNumber);
  }


}
