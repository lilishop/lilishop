package cn.lili.modules.promotion.entity.vos;

import cn.hutool.core.text.CharSequenceUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 积分商品查询通用类
 *
 * @author paulG
 * @since 2021/1/13
 **/
@EqualsAndHashCode(callSuper = true)
@Data
public class PointsGoodsSearchParams extends BasePromotionsSearchParams {

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "商品skuId")
    private String skuId;

    @ApiModelProperty(value = "积分商品分类编号")
    private String pointsGoodsCategoryId;

    @ApiModelProperty(value = "是否为推荐商品")
    private Boolean recommend;

    @ApiModelProperty(value = "积分,可以为范围，如10_1000")
    private String points;


    @Override
    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = super.queryWrapper();
        if (CharSequenceUtil.isNotEmpty(goodsName)) {
            queryWrapper.eq("gs.goods_name", goodsName);
        }
        if (CharSequenceUtil.isNotEmpty(skuId)) {
            queryWrapper.eq("pg.sku_id", skuId);
        }
        if (CharSequenceUtil.isNotEmpty(pointsGoodsCategoryId)) {
            queryWrapper.eq("pg.points_goods_category_id", pointsGoodsCategoryId);
        }
        if (CharSequenceUtil.isNotEmpty(points)) {
            String[] s = points.split("_");
            if (s.length > 1) {
                queryWrapper.between("pg.points", s[0], s[1]);
            } else {
                queryWrapper.eq("pg.points", s[0]);
            }
        }
        if (recommend != null) {
            queryWrapper.eq("gs.recommend", recommend);
        }
        return queryWrapper;
    }

}
