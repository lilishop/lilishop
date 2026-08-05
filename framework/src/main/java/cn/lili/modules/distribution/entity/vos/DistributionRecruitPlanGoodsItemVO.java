package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 分销招募指定商品展示项
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionRecruitPlanGoodsItemVO {

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "商品编码")
    private String sn;

    @Schema(description = "SKU规格信息")
    private String simpleSpecs;

    @Schema(description = "商品缩略图")
    private String thumbnail;

    @Schema(description = "商品价格")
    private Double price;
}
