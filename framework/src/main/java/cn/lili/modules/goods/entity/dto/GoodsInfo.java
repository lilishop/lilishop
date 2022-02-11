package cn.lili.modules.goods.entity.dto;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.goods.entity.dos.Commodity;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 微信小程序直播商品DTO
 *
 * @author Bulbasaur
 * @since 2021/5/17 11:03 上午
 */
@Data
@NoArgsConstructor
public class GoodsInfo {

    @ApiModelProperty(value = "图片mediaID")
    private String coverImgUrl;

    @ApiModelProperty(value = "商品名称")
    private String name;

    /**
     * 1：一口价（只需要传入price，price2不传）
     * 2：价格区间（price字段为左边界，price2字段为右边界，price和price2必传）
     * 3：显示折扣价（price字段为原价，price2字段为现价， price和price2必传
     */
    @ApiModelProperty(value = "价格类型")
    private Integer priceType;

    @ApiModelProperty(value = "价格")
    private Double price;

    @ApiModelProperty(value = "价格2")
    private Double price2;

    @ApiModelProperty(value = "商品详情页的小程序路径")
    private String url;

    public GoodsInfo(Commodity commodity) {
        BeanUtil.copyProperties(commodity, this);
    }
}
