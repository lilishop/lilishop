package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 商品区间销售聚合(同比环比中间结果)
 *
 * @author Bulbasaur
 * @since 2026/07/20
 */
@Data
@Schema(description = "商品区间销售聚合")
public class GoodsSalesRangeVO {

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "店铺ID")
    private String storeId;

    @Schema(description = "销售金额")
    private Double salesAmount;

    @Schema(description = "销售数量")
    private Long salesNum;
}
