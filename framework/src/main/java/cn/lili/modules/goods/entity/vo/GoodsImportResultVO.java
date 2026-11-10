package cn.lili.modules.goods.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * 商品批量导入v2结果
 *
 * @author Bulbasaur
 * @since 2026-10-20
 */
@Data
@Schema(description = "商品批量导入v2结果")
public class GoodsImportResultVO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 导入成功的商品数量（按商品组计，一个多规格商品计 1）
     */
    @Schema(description = "导入成功的商品数量")
    private Integer successCount = 0;

    /**
     * 导入失败的商品数量（按商品组计）
     */
    @Schema(description = "导入失败的商品数量")
    private Integer failCount = 0;

    /**
     * 失败商品明细，粒度为商品组而非单行
     */
    @Schema(description = "失败商品明细")
    private List<FailGoods> failList = new ArrayList<>();

    /**
     * 导入失败商品，失败粒度为商品组：同一商品序号下的所有 SKU 行要么全部成功，要么整组失败
     *
     * @author Bulbasaur
     * @since 2026-10-20
     */
    @Data
    @Schema(description = "导入失败商品")
    public static class FailGoods implements Serializable {

        private static final long serialVersionUID = 1L;

        /**
         * 商品序号，对应 Excel 第 A 列的分组标识
         */
        @Schema(description = "商品序号")
        private Integer goodsNo;

        /**
         * 该商品组涉及的 Excel 行范围，如 "3-5"
         */
        @Schema(description = "涉及行范围")
        private String rows;

        /**
         * 失败原因
         */
        @Schema(description = "失败原因")
        private String reason;
    }
}
