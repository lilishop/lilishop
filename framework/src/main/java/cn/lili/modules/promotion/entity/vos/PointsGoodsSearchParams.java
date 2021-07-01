package cn.lili.modules.promotion.entity.vos;

import cn.hutool.core.convert.Convert;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;

import java.util.regex.Pattern;

/**
 * 积分商品查询通用类
 *
 * @author paulG
 * @date 2021/1/13
 **/
@Data
public class PointsGoodsSearchParams {

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

    /**
     * @see PromotionStatusEnum
     */
    @ApiModelProperty(value = "活动状态")
    private String promotionStatus;


    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
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
        if (CharSequenceUtil.isNotEmpty(promotionStatus)) {
            queryWrapper.eq("pg.promotion_status", promotionStatus);
        }
        return queryWrapper;
    }


    public Query mongoQuery() {
        Query query = new Query();
        if (CharSequenceUtil.isNotEmpty(goodsName)) {
            Pattern pattern = Pattern.compile("^.*" + goodsName + ".*$", Pattern.CASE_INSENSITIVE);
            query.addCriteria(Criteria.where("goodsSku.goodsName").regex(pattern));
        }
        if (CharSequenceUtil.isNotEmpty(skuId)) {
            query.addCriteria(Criteria.where("skuId").is(skuId));
        }
        if (CharSequenceUtil.isNotEmpty(pointsGoodsCategoryId)) {
            query.addCriteria(Criteria.where("pointsGoodsCategoryId").is(pointsGoodsCategoryId));
        }
        if (CharSequenceUtil.isNotEmpty(points)) {
            String[] s = points.split("_");
            if (s.length > 1) {
                query.addCriteria(Criteria.where("points").gte(Convert.toInt(s[0])).lte(Convert.toInt(s[1])));
            } else {
                query.addCriteria(Criteria.where("points").gte(Convert.toInt(s[0])));
            }
        }
        if (recommend != null) {
            query.addCriteria(Criteria.where("goodsSku.recommend").is(recommend));
        }
        if (CharSequenceUtil.isNotEmpty(promotionStatus)) {
            query.addCriteria(Criteria.where("promotionStatus").is(promotionStatus));
        }
        query.addCriteria(Criteria.where("deleteFlag").is(false));
        return query;
    }

}
