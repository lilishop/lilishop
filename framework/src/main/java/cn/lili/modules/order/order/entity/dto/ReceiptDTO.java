package cn.lili.modules.order.order.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 发票子内容
 *
 * @author Bulbasaur
 * @since 2020/11/28 11:44
 */
@Data
public class ReceiptDTO {

    @ApiModelProperty(value = "发票ID")
    private String receiptId;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "规格")
    private String specs;

    @ApiModelProperty(value = "数量")
    private Integer num;

    @ApiModelProperty(value = "单价")
    private Double goodPrice;

    @ApiModelProperty(value = "小计")
    private Double subtotal;
}
