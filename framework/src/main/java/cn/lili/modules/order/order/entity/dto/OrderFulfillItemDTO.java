package cn.lili.modules.order.order.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 统一发货行项
 *
 * @author Mike
 * @since 2026-08-13
 */
@Data
public class OrderFulfillItemDTO {

    @Schema(description = "订单货物 ID（li_order_item.id）")
    private String orderItemId;

    @Schema(description = "本次发货数量；省略时默认为剩余待发数量")
    private Integer deliveryNum;
}
