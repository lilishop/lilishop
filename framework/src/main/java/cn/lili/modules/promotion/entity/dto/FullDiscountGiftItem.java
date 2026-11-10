package cn.lili.modules.promotion.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 满赠活动阶梯内的单个赠品配置及快照项
 * <p>
 * 用于记录满赠活动每个阶梯所选的商品 SKU 及赠送件数（每个阶梯最多配置 3 个赠品，每个赠品赠送数量为 1~10 件）。
 * 同时用于购物车渲染及订单支付后生成赠品子单时的赠品明细传递。
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "满赠阶梯赠品项")
public class FullDiscountGiftItem implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "赠品SKU ID")
    private String skuId;

    @Schema(description = "赠品商品ID")
    private String goodsId;

    @Schema(description = "赠品名称")
    private String goodsName;

    @Schema(description = "赠品缩略图")
    private String thumbnail;

    @Schema(description = "赠品规格简述")
    private String simpleSpecs;

    @Schema(description = "赠品商品类型")
    private String goodsType;

    @Schema(description = "赠品原价（元）")
    private Double price;

    @Schema(description = "赠送数量（1~10件）")
    private Integer num;

    /**
     * 获取有效的赠送数量，为空或小于 1 时默认返回 1
     *
     * @return 赠送数量
     */
    public Integer validNum() {
        return (num == null || num < 1) ? 1 : num;
    }
}
