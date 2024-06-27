package cn.lili.modules.goods.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 商品库存DTO
 *
 * @author paulG
 * @since 2020/12/23
 **/
@Data
public class GoodsSkuStockDTO {

    @ApiModelProperty(value = "商品id")
    private String goodsId;

    @ApiModelProperty(value = "商品skuId")
    private String skuId;

    @ApiModelProperty(value = "库存")
    private Integer quantity;

    @ApiModelProperty(value = "预警库存")
    private Integer alertQuantity;

    @ApiModelProperty(value = "规格信息")
    private String simpleSpecs;

    @ApiModelProperty(value = "商品编号")
    private String sn;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    /**
     * @see cn.lili.modules.goods.entity.enums.GoodsStockTypeEnum
     */
    @ApiModelProperty(value = "类型")
    private String type;
}
