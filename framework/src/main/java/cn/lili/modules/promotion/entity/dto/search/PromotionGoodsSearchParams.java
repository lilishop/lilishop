package cn.lili.modules.promotion.entity.dto.search;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.Arrays;
import java.util.List;

/**
 * 促销商品查询通用类
 *
 * @author paulG
 * @since 2021/2/21
 **/
@EqualsAndHashCode(callSuper = true)
@Data
public class PromotionGoodsSearchParams extends BasePromotionsSearchParams {

    @ApiModelProperty(value = "促销活动id")
    private String promotionId;

    @ApiModelProperty(value = "促销类型")
    private String promotionType;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "商品分类路径")
    private String categoryPath;

    @ApiModelProperty(value = "商品SkuId")
    private String skuId;

    @ApiModelProperty(value = "商品SkuIds")
    private List<String> skuIds;

    @ApiModelProperty(value = "促销活动id")
    private List<String> promotionIds;


    @Override
    public <T> QueryWrapper<T> queryWrapper() {
        if (CharSequenceUtil.isEmpty(this.getScopeType())){
            this.setScopeType(PromotionsScopeTypeEnum.PORTION_GOODS.name());
        }
        QueryWrapper<T> queryWrapper = super.queryWrapper();
        if (CharSequenceUtil.isNotEmpty(promotionId)) {
            queryWrapper.eq("promotion_id", promotionId);
        }
        if (CharSequenceUtil.isNotEmpty(goodsName)) {
            queryWrapper.like("goods_name", goodsName);
        }
        if (CharSequenceUtil.isNotEmpty(promotionType)) {
            queryWrapper.eq("promotion_type", promotionType);
        }
        if (CharSequenceUtil.isNotEmpty(categoryPath)) {
            queryWrapper.like("category_path", categoryPath);
        }
        if (CharSequenceUtil.isNotEmpty(skuId)) {
            queryWrapper.in("sku_id", Arrays.asList(skuId.split(",")));
        }
        if (skuIds != null && !skuIds.isEmpty()) {
            queryWrapper.in("sku_id", skuIds);
        }
        if (promotionIds != null && promotionIds.isEmpty()) {
            queryWrapper.in("promotion_id", promotionIds);
        }
        return queryWrapper;
    }

}
