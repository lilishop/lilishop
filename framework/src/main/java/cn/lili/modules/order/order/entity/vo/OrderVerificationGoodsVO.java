package cn.lili.modules.order.order.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 待核销货物
 *
 * @author Bulbasaur
 * @since 2026-09-23
 */
@Data
@Schema(description = "待核销货物")
public class OrderVerificationGoodsVO {

    @Schema(description = "订单货物编号")
    private String orderItemSn;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "商品图片")
    private String image;

    @Schema(description = "规格")
    private String specs;

    @Schema(description = "本次核销数量")
    private Integer num;
}
