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

    @ApiModelProperty(value = "商品skuId")
    private String skuId;

    @ApiModelProperty(value = "库存")
    private Integer quantity;


}
