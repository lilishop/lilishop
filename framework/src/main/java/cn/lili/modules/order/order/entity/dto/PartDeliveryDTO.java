package cn.lili.modules.order.order.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 部分发货
 */
@Data
public class PartDeliveryDTO {

    @ApiModelProperty(value = "订单货物Id")
    private String orderItemId;

    @ApiModelProperty(value = "发货数量")
    private Integer deliveryNum;


}
