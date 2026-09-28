package cn.lili.modules.order.order.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

/**
 * 核销前预览，供二次确认
 *
 * @author Bulbasaur
 * @since 2026-09-23
 */
@Data
@Schema(description = "核销确认预览")
public class OrderVerificationPreviewVO {

    @Schema(description = "核销码")
    private String code;

    @Schema(description = "订单编号")
    private String orderSn;

    @Schema(description = "待核销货物")
    private List<OrderVerificationGoodsVO> goods;
}
