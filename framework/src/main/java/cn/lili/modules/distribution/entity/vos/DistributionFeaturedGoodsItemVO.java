package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 主推商品展示项
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionFeaturedGoodsItemVO {

    @Schema(description = "分销商品ID")
    private String id;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "规格ID")
    private String skuId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "规格")
    private String specs;

    @Schema(description = "商品图片")
    private String thumbnail;

    @Schema(description = "商品价格")
    private Double price;

    @Schema(description = "佣金金额")
    private Double commission;

    @Schema(description = "销量")
    private Integer salesCount;
}
