package cn.lili.modules.order.order.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 订单批量发货DTO
 * @author Bulbasaur
 * @since 2021/5/26 4:21 下午
 *
 */
@Data
public class OrderBatchDeliverDTO {

    @ApiModelProperty(value = "订单SN")
    private String orderSn;

    @ApiModelProperty(value = "物流公司ID")
    private String logisticsId;

    @ApiModelProperty(value = "物流公司名称")
    private String logisticsName;

    @ApiModelProperty(value = "发货单号")
    private String logisticsNo;

}
