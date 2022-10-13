package cn.lili.modules.goods.entity.dto;

import cn.lili.modules.goods.entity.dos.DraftGoods;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.Valid;
import java.util.List;
import java.util.Map;

/**
 * 草稿商品DTO
 *
 * @author paulG
 * @since 2020/12/22
 **/
@Data
public class DraftGoodsDTO extends DraftGoods {

    private static final long serialVersionUID = 5255666163196674178L;

    @ApiModelProperty(value = "商品参数")
    @Valid
    private List<GoodsParamsDTO> goodsParamsDTOList;

    @ApiModelProperty(value = "商品图片")
    private List<String> goodsGalleryList;

    @ApiModelProperty(value = "sku列表")
    @Valid
    private List<Map<String, Object>> skuList;

    /**
     * 批发商品规则
     */
    @ApiModelProperty(value = "批发商品规则")
    private List<WholesaleDTO> wholesaleList;

}
