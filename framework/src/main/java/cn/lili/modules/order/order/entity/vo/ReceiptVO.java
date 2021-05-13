package cn.lili.modules.order.order.entity.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 发票
 *
 * @author Bulbasaur
 * @date 2020/11/28 11:38
 */
@Data
@ApiModel(value = "发票")
public class ReceiptVO {

    @ApiModelProperty(value = "发票抬头")
    private String receiptTitle;

    @ApiModelProperty(value = "纳税人识别号")
    private String taxpayerId;

    @ApiModelProperty(value = "发票内容")
    private String receiptContent;

}
