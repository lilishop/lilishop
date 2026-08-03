package cn.lili.modules.goods.entity.dto.search;

import cn.lili.common.vo.PageVO;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 卡池列表查询参数
 *
 * @author Mike
 * @date 2026-07-31
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "卡池查询参数")
public class CardKeySearchParams extends PageVO {

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "商品 ID")
    private String goodsId;

    @Schema(description = "状态 UNUSED/ALLOCATED/VOIDED")
    private String status;

    @Schema(description = "卡号模糊搜索")
    private String cardNo;

    @Schema(description = "导入时间起")
    private String createTimeStart;

    @Schema(description = "导入时间止")
    private String createTimeEnd;

    @Schema(description = "店铺 ID（内部使用）")
    private String storeId;
}
