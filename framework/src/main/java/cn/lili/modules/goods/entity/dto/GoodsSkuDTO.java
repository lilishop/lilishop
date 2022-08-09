package cn.lili.modules.goods.entity.dto;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * @author paulG
 * @since 2022/6/13
 **/
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
public class GoodsSkuDTO extends GoodsSku {

    private static final long serialVersionUID = 6600436187015048097L;

    @ApiModelProperty(value = "商品参数json")
    private String params;

}
