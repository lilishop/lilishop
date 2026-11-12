package cn.lili.modules.stock.entity.params;

import cn.lili.common.vo.PageVO;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 库存流水分页查询参数
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Schema(description = "库存流水查询参数")
public class StockFlowSearchParams extends PageVO {

    private static final long serialVersionUID = 1L;

    @Schema(description = "店铺ID", hidden = true)
    private String storeId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "变动类型")
    private String flowType;

    @Schema(description = "变动方向")
    private String direction;

    @Schema(description = "业务单号")
    private String bizSn;

    @Schema(description = "起始时间")
    private String startDate;

    @Schema(description = "结束时间")
    private String endDate;
}
