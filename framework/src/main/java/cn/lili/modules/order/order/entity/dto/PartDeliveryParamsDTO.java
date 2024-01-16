package cn.lili.modules.order.order.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * 部分发货参数封装
 *
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.0
 * @Description:
 * @since 2022/10/29 17:52
 */
@Data
public class PartDeliveryParamsDTO {

    @ApiModelProperty(value = "订单号")
    private String orderSn;

    @ApiModelProperty(value = "发货单号")
    private String logisticsNo;

    @ApiModelProperty(value = "发货方式")
    private String logisticsId;

    @ApiModelProperty(value = "物流详细")
    private List<PartDeliveryDTO> partDeliveryDTOList;
}
