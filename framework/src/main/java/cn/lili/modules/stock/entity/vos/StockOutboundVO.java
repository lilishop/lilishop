package cn.lili.modules.stock.entity.vos;

import cn.lili.modules.stock.entity.dos.StockOutbound;
import cn.lili.modules.stock.entity.dos.StockOutboundItem;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.List;

/**
 * 商家出库单视图对象，包含主单基础信息及明细商品列表
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "出库单详情视图")
public class StockOutboundVO extends StockOutbound {

    private static final long serialVersionUID = 1L;

    @Schema(description = "明细列表")
    private List<StockOutboundItem> items;
}
