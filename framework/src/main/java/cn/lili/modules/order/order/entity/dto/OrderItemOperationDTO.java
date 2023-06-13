package cn.lili.modules.order.order.entity.dto;

import cn.hutool.core.date.DateTime;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 订单Item操作DTO
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class OrderItemOperationDTO {

    @ApiModelProperty(value = "订单完成时间")
    DateTime receiveTime;

    @ApiModelProperty(value = "订单售后状态")
    String afterSaleStatus;

    @ApiModelProperty(value = "订单评价状态")
    String commentStatus;

    @ApiModelProperty(value = "订单投诉状态")
    String complainStatus;

}
