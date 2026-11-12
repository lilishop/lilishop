package cn.lili.modules.stock.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 商家出库单商品明细实体类
 * 记录单张出库单中每个商品SKU的出库数量与规格快照
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("li_stock_outbound_item")
@Schema(description = "出库单明细")
public class StockOutboundItem extends BaseIdEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "出库单ID")
    private String outboundId;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "规格快照")
    private String specs;

    @Schema(description = "计量单位")
    private String goodsUnit;

    @Schema(description = "出库数量")
    private Integer quantity;

    @Schema(description = "明细备注")
    private String itemRemark;
}
