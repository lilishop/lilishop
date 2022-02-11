package cn.lili.modules.order.order.entity.dto;

import cn.lili.modules.order.order.entity.dos.Receipt;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 订单发票
 *
 * @author lili
 * @since 2020/11/28 11:38
 */
@Data
@ApiModel(value = "订单发票")
public class OrderReceiptDTO extends Receipt {

    @ApiModelProperty(value = "订单状态")
    private String orderStatus;

}
