package cn.lili.modules.distribution.entity.vos;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 分销商品信息
 *
 * @author pikachu
 * @since 2020-03-26 09:04:53
 */
@Data
public class DistributionGoodsVO {

    @ApiModelProperty(value = "分销商品ID")
    private String id;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "规格")
    private String specs;

    @ApiModelProperty(value = "库存")
    private Integer quantity;

    @ApiModelProperty(value = "商品图片")
    private String thumbnail;

    @ApiModelProperty(value = "商品价格")
    private Double price;

    @ApiModelProperty(value = "商品编号")
    private String sn;

    @ApiModelProperty(value = "商品ID")
    private String goodsId;

    @ApiModelProperty(value = "规格ID")
    private String skuId;

    @ApiModelProperty(value = "店铺名称")
    private String storeName;

    @ApiModelProperty(value = "佣金金额")
    private Double commission;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "添加时间")
    private Date createTime;

}
