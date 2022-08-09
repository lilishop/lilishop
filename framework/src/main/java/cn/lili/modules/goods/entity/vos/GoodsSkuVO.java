package cn.lili.modules.goods.entity.vos;

import cn.hutool.core.bean.BeanUtil;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * 商品规格VO
 *
 * @author paulG
 * @since 2020-02-26 23:24:13
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
public class GoodsSkuVO extends GoodsSku {

    private static final long serialVersionUID = -7651149660489332344L;

    @ApiModelProperty(value = "规格列表")
    private List<SpecValueVO> specList;

    @ApiModelProperty(value = "商品图片")
    private List<String> goodsGalleryList;

    public GoodsSkuVO(GoodsSku goodsSku) {
        BeanUtil.copyProperties(goodsSku, this);
    }
}

