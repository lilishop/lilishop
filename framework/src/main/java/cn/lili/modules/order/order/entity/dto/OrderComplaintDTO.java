package cn.lili.modules.order.order.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotBlank;

/**
 * 交易投诉DTO
 *
 * @author Bulbasaur
 * @since 2021/1/7 19:39
 */
@Data
public class OrderComplaintDTO {

    @NotBlank
    @ApiModelProperty(value = "投诉主题")
    private String complainTopic;

    @NotBlank
    @ApiModelProperty(value = "投诉内容")
    private String content;

    @ApiModelProperty(value = "投诉凭证图片")
    private String images;

    @NotBlank
    @ApiModelProperty(value = "订单号")
    private String orderSn;

    @NotBlank
    @ApiModelProperty(value = "商品id")
    private String goodsId;

    @NotBlank
    @ApiModelProperty(value = "sku主键")
    private String skuId;
}
