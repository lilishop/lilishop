package cn.lili.modules.order.order.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

/**
 * 统一发货请求
 *
 * @author Mike
 * @since 2026-08-13
 */
@Data
public class OrderFulfillParamsDTO {

    @Schema(description = "订单号")
    private String orderSn;

    @Schema(description = "履约模式：WHOLE 整单发货 / SPLIT 拆单发货 / NO_DELIVERY 无需配送")
    private String fulfillmentMode;

    @Schema(description = "物流公司 ID（快递模式必填）")
    private String logisticsId;

    @Schema(description = "运单号（快递模式必填）")
    private String logisticsNo;

    @Schema(description = "无需配送说明（NO_DELIVERY 可选）")
    private String deliveryRemark;

    @Schema(description = "拆单发货行项；WHOLE/NO_DELIVERY 可省略由服务端补全")
    private List<OrderFulfillItemDTO> items;
}
