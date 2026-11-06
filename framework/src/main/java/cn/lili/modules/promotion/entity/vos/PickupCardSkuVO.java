package cn.lili.modules.promotion.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 提货卡候选商品展示，避免向买家返回商品内部成本及其他敏感字段。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
@Data
@Schema(description = "提货卡候选商品")
public class PickupCardSkuVO {

    @Schema(description = "商品 SKU ID，对应 li_goods_sku.id")
    private String id;

    @Schema(description = "商品名称；商品已删除时展示失效提示")
    private String goodsName;

    @Schema(description = "商品缩略图地址")
    private String thumbnail;

    @Schema(description = "SKU 规格的既有 JSON 展示数据")
    private String specs;

    @Schema(description = "所属店铺名称，用于跨店铺兑换前确认")
    private String storeName;

    @Schema(description = "当前销售单价，单位元；兑换订单以提交时重新读取的单价为准")
    private Double price;

    @Schema(description = "是否为有效上架实物商品且当前有库存，不代表库存已预占")
    private Boolean available;
}
