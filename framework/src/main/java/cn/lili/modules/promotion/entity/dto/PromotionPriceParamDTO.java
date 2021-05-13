package cn.lili.modules.promotion.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 促销价格参数数据传输对象
 *
 * @author paulG
 * @date 2020/11/17
 **/
@Data
public class PromotionPriceParamDTO {

    @ApiModelProperty(value = "商品SkuId")
    private String skuId;

    @ApiModelProperty(value = "购买数量")
    private Integer num;

    @ApiModelProperty(value = "拼团id 如果是拼团购买 此值为拼团活动id，当pintuanId为空，则表示普通购买（或者拼团商品，单独购买）")
    private String pintuanId;
}
