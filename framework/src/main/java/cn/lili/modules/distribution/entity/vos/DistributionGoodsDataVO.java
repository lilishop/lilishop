package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 分销商品数据VO
 *
 * @author pikachu
 */
@Data
public class DistributionGoodsDataVO {

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "分享人数")
    private Long shareDistributorCount;

    @Schema(description = "分享次数")
    private Long shareCount;

    @Schema(description = "访问人数")
    private Long visitVisitorCount;

    @Schema(description = "成交金额")
    private BigDecimal dealAmount;

    @Schema(description = "支出佣金")
    private BigDecimal commissionAmount;

    @Schema(description = "访问转化率")
    private BigDecimal visitConversionRate;
}
