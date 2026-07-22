package cn.lili.modules.statistics.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 商品统计VO
 *
 * @author Bulbasaur
 * @since 2020/12/9 14:25
 */
@Data
public class GoodsStatisticsDataVO {

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "下单笔数")
    private Long orderNum;

    @Schema(description = "销售数量")
    private String num;

    @Schema(description = "销售金额")
    private Double price;
}
