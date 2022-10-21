package cn.lili.common.fulu.model;

/**
 * 商品模板信息input dto
 *
 * @Auther: chenYing
 * @Date: 2019/8/19 0019 16:01
 */
public class InputProductTemplateDto extends CommonRequest {

  public InputProductTemplateDto() {
    super();
    setMethod("fulu.goods.template.get");
  }

  /**
   * 商品模板编号
   *
   * @return String
   */
  public String getTemplateId() {
    Object value = getBizContentValue("template_id");
    return value != null ? String.valueOf(value) : null;
  }

  public void setTemplateId(String templateId) {
    setBizContent("template_id", templateId);
  }
}
