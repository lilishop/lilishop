package cn.lili.modules.promotion.entity.dos;

import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;


/**
 * 积分商品实体类
 *
 * @author paulG
 * @since 2020-03-19 10:44 上午
 **/
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("li_points_goods")
@ApiModel(value = "积分商品")
@AllArgsConstructor
@NoArgsConstructor
public class PointsGoods extends BasePromotions {

    private static final long serialVersionUID = 1313207311581661571L;

    @ApiModelProperty(value = "商品编号")
    private String goodsId;

    @ApiModelProperty(value = "商品sku编号")
    private String skuId;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "商品原价")
    private Double originalPrice;

    @ApiModelProperty(value = "结算价格")
    private Double settlementPrice;

    @ApiModelProperty(value = "积分商品分类编号")
    private String pointsGoodsCategoryId;

    @ApiModelProperty(value = "分类名称")
    private String pointsGoodsCategoryName;

    @ApiModelProperty(value = "缩略图")
    private String thumbnail;

    @ApiModelProperty(value = "活动库存数量")
    private Integer activeStock;

    @ApiModelProperty(value = "兑换积分")
    private Long points;

}
