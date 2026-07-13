package cn.lili.modules.search.entity.dto;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.utils.RegularUtil;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author paulG
 * @since 2020/10/15
 **/
@Data
public class GoodsSearchDTO {

    @Schema(title = "商品Ids")
    private String ids;

    @Schema(description = "关键字")
    private String keyword;

    @Schema(description = "分类")
    private String categoryId;

    @Schema(description = "品牌,可以多选 品牌Id@品牌Id@品牌Id")
    private String brandId;

    @Schema(description = "是否为推荐商品")
    private Boolean recommend;

    @Schema(description = "价格", example = "10_30")
    private String price;

    @Schema(description = "属性:参数名_参数值@参数名_参数值", example = "屏幕类型_LED@屏幕尺寸_15英寸")
    private String prop;

    @Schema(description = "规格项列表")
    private List<String> nameIds;

    @Schema(description = "卖家id，搜索店铺商品的时候使用")
    private String storeId;

    @Schema(description = "商家分组id，搜索店铺商品的时候使用")
    private String storeCatId;

    @Schema(hidden = true)
    private Map<String, List<String>> notShowCol = new HashMap<>();

    @Schema(description = "当前商品skuId,根据当前浏览的商品信息来给用户推荐可能喜欢的商品")
    private String currentGoodsId;

    /**
     * @see cn.lili.common.enums.PromotionTypeEnum
     */
    @Schema(description = "促销活动类型")
    private String promotionType;

    @Schema(description = "促销活动id")
    private String promotionsId;

    /**
     * @see cn.lili.modules.goods.entity.enums.GoodsTypeEnum
     */
    @Schema(description = "商品类型")
    private String goodsType;

    @Schema(description = "销售模式")
    private String salesModel;

    /**
     * @see cn.lili.modules.goods.entity.enums.GoodsTypeEnum
     */
    @Schema(description = "除了当前商品类型之外")
    private String neGoodsType;

    @Schema(description = "除了销售模式当前销售模式之外")
    private String neSalesModel;

    //过滤搜索关键字
    public String getKeyword() {
        if (CharSequenceUtil.isNotEmpty(keyword)) {
            RegularUtil.replace(this.keyword);
        }
        return keyword;
    }
}
