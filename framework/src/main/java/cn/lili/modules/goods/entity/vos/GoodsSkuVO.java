package cn.lili.modules.goods.entity.vos;

import cn.hutool.core.bean.BeanUtil;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import io.swagger.v3.oas.annotations.media.Schema;
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

    @Schema(description = "规格列表")
    private List<SpecValueVO> specList;

    @Schema(description = "商品图片")
    private List<String> goodsGalleryList;

    /** 卡池可售数（UNUSED 且未删），E_COUPON SKU 详情/列表展示，与 quantity 镜像同步 */
    @Schema(description = "卡池可售数（E_COUPON）")
    private Integer poolStock;

    public GoodsSkuVO(GoodsSku goodsSku) {
        BeanUtil.copyProperties(goodsSku, this);
    }
}

