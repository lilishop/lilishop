package cn.lili.modules.promotion.entity.vos;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 促销商品查询通用类
 *
 * @author paulG
 * @date 2021/2/21
 **/
@Data
public class PromotionGoodsSearchParams {

    @ApiModelProperty(value = "促销活动id")
    private String promotionId;

    @ApiModelProperty(value = "促销类型")
    private String promotionType;

    @ApiModelProperty(value = "促销状态")
    private String promotionStatus;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "商品分类路径")
    private String categoryPath;

    @ApiModelProperty(value = "开始时间")
    private Long startTime;

    @ApiModelProperty(value = "结束时间")
    private Long endTime;


    public LambdaQueryWrapper<PromotionGoods> queryWrapper() {
        LambdaQueryWrapper<PromotionGoods> queryWrapper = new LambdaQueryWrapper<>();
        if (CharSequenceUtil.isNotEmpty(promotionId)) {
            queryWrapper.eq(PromotionGoods::getPromotionId, promotionId);
        }
        if (CharSequenceUtil.isNotEmpty(goodsName)) {
            queryWrapper.like(PromotionGoods::getGoodsName, goodsName);
        }
        if (CharSequenceUtil.isNotEmpty(promotionType)) {
            queryWrapper.eq(PromotionGoods::getPromotionType, promotionType);
        }
        if (CharSequenceUtil.isNotEmpty(promotionStatus)) {
            queryWrapper.eq(PromotionGoods::getPromotionStatus, promotionStatus);
        }
        if (CharSequenceUtil.isNotEmpty(categoryPath)) {
            queryWrapper.like(PromotionGoods::getCategoryPath, categoryPath);
        }
        if (startTime != null) {
            queryWrapper.ge(PromotionGoods::getStartTime, new Date(startTime));
        }
        if (endTime != null) {
            queryWrapper.ge(PromotionGoods::getEndTime, new Date(endTime));
        }
        queryWrapper.eq(PromotionGoods::getDeleteFlag, false);
        return queryWrapper;
    }

}
