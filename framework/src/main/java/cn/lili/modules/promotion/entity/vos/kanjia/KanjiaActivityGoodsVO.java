package cn.lili.modules.promotion.entity.vos.kanjia;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 砍价商品视图对象
 *
 * @author paulG
 * @date 2021/1/13
 **/
@Data
public class KanjiaActivityGoodsVO {

    @ApiModelProperty(value = "商品规格详细信息")
    private GoodsSku goodsSku;

    @ApiModelProperty(value = "最低购买金额")
    private Double purchasePrice;

    public Double getPurchasePrice() {
        if (purchasePrice < 0) {
            return 0D;
        }
        return purchasePrice;
    }

    @ApiModelProperty(value = "活动库存")
    private Integer stock;

}
