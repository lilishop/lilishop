package cn.lili.modules.broadcast.entity.vos;

import cn.lili.modules.broadcast.entity.dos.Commodity;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 直播商品VO
 *
 * @author Bulbasaur
 * @date: 2021/5/26 6:09 下午
 */
@Data
public class CommodityVO extends Commodity {

    @ApiModelProperty(value = "SKU库存")
    private Integer quantity;

    @ApiModelProperty(value = "店铺名称")
    private String storeName;
}
