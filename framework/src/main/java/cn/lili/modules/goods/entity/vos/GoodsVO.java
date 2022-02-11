package cn.lili.modules.goods.entity.vos;

import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * 商品VO
 *
 * @author pikachu
 * @since 2020-02-26 23:24:13
 */
@Data
public class GoodsVO extends Goods {

    private static final long serialVersionUID = 6377623919990713567L;

    @ApiModelProperty(value = "分类名称")
    private List<String> categoryName;

    @ApiModelProperty(value = "商品参数")
    private List<GoodsParamsDTO> goodsParamsDTOList;

    @ApiModelProperty(value = "商品图片")
    private List<String> goodsGalleryList;

    @ApiModelProperty(value = "sku列表")
    private List<GoodsSkuVO> skuList;
}
