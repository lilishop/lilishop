package cn.lili.modules.goods.entity.dto;

import cn.lili.modules.goods.entity.dos.Category;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * 商品导入DTO
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class GoodsImportDTO {

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "商品卖点")
    private String sellingPoint;

    @ApiModelProperty(value = "商品分类")
    private Category category;

    @ApiModelProperty(value = "运费模板")
    private String template;

    @ApiModelProperty(value = "计量单位")
    private String goodsUnit;

    @ApiModelProperty(value = "发布状态")
    private Boolean release;

    @ApiModelProperty(value = "商品图片")
    private List<Map<String, String>> images;
    private List<String> goodsGalleryList;

    @ApiModelProperty(value = "成本价")
    private Double cost;

    @ApiModelProperty(value = "销售价")
    private Double price;

    @ApiModelProperty(value = "库存")
    private Integer quantity;

    @ApiModelProperty(value = "重量")
    private Double weight;

    @ApiModelProperty(value = "货号")
    private String sn;

    @ApiModelProperty(value = "详情")
    private String intro;

    @ApiModelProperty(value = "规格项")
    private String skuKey;

    @ApiModelProperty(value = "规格值")
    private String skuValue;


}
