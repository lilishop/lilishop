package cn.lili.modules.promotion.entity.vos.kanjia;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 砍价商品视图对象
 *
 * @author paulG
 * @date 2021/1/13
 **/
@Data
public class KanjiaActivityGoodsListVO {

    @ApiModelProperty(value = "砍价活动商品id")
    private String id;

    @ApiModelProperty(value = "货品名称")
    private String goodsName;

    @ApiModelProperty(value = "缩略图")
    private String thumbnail;

    @ApiModelProperty(value = "最低购买金额")
    private Double purchasePrice;

    @ApiModelProperty(value = "活动库存")
    private Integer stock;

}
