package cn.lili.modules.stock.entity.params;

import cn.lili.common.vo.PageVO;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 商家出库单分页查询参数
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Schema(description = "出库单查询参数")
public class StockOutboundSearchParams extends PageVO {

    private static final long serialVersionUID = 1L;

    @Schema(description = "店铺ID", hidden = true)
    private String storeId;

    @Schema(description = "出库单号")
    private String outboundSn;

    @Schema(description = "单据状态")
    private String status;

    @Schema(description = "起始时间")
    private String startDate;

    @Schema(description = "结束时间")
    private String endDate;
}
