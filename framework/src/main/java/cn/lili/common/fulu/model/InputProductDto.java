package cn.lili.common.fulu.model;

/**
 * 商品信息input dto
 *
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 16:04
 */
public class InputProductDto extends CommonRequest {

  public InputProductDto() {
    super();
    setMethod("fulu.goods.info.get");
  }

  /**
   * 商品编号
   *
   * @return String
   */
  public String getProductId() {
    Object value = getBizContentValue("product_id");
    return value != null ? String.valueOf(value) : null;
  }

  public void setProductId(String productId) {
    setBizContent("product_id", productId);
  }

}
