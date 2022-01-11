package cn.lili.modules.promotion.entity.dto;


import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityGoods;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;

/**
 * 砍价活动商品DTO
 *
 * @author qiuqiu
 * @since 2020/8/21
 **/
@EqualsAndHashCode(callSuper = true)
@Data
public class KanjiaActivityGoodsDTO extends KanjiaActivityGoods implements Serializable {


    private static final long serialVersionUID = 1969340823809319805L;

    @ApiModelProperty(value = "商品规格详细信息")
    private GoodsSku goodsSku;

}
