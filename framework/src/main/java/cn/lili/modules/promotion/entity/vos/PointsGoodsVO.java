package cn.lili.modules.promotion.entity.vos;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.promotion.entity.dos.PointsGoods;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 积分商品视图对象
 *
 * @author paulG
 * @since 2021/1/13
 **/
@EqualsAndHashCode(callSuper = true)
@Data
public class PointsGoodsVO extends PointsGoods {

    private static final long serialVersionUID = -5163709626742905057L;

    @ApiModelProperty(value = "商品规格详细信息")
    private GoodsSku goodsSku;

}
