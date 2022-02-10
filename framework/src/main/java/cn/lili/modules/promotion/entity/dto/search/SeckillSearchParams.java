package cn.lili.modules.promotion.entity.dto.search;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.promotion.entity.enums.PromotionsApplyStatusEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;
import java.util.Arrays;

/**
 * 秒杀活动查询通用类
 *
 * @author paulG
 * @since 2020/8/21
 **/
@EqualsAndHashCode(callSuper = true)
@Data
public class SeckillSearchParams extends BasePromotionsSearchParams implements Serializable {

    private static final long serialVersionUID = -4052716630253333681L;

    @ApiModelProperty(value = "秒杀活动活动编号")
    private String seckillId;

    @ApiModelProperty(value = "活动名称")
    private String promotionName;

    @ApiModelProperty(value = "时刻")
    private Integer timeLine;

    @ApiModelProperty(value = "商家id")
    private String[] storeIds;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "商家编号")
    private String skuId;

    /**
     * @see PromotionsApplyStatusEnum
     */
    @ApiModelProperty(value = "APPLY(\"申请\"), PASS(\"通过\"), REFUSE(\"拒绝\")")
    private String promotionApplyStatus;

    @Override
    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = super.queryWrapper();
        if (CharSequenceUtil.isNotEmpty(goodsName)) {
            queryWrapper.like("goods_name", goodsName);
        }
        if (CharSequenceUtil.isNotEmpty(promotionName)) {
            queryWrapper.like("promotion_name", promotionName);
        }
        if (CharSequenceUtil.isNotEmpty(seckillId)) {
            queryWrapper.eq("seckill_id", seckillId);
        }
        if (storeIds != null) {
            queryWrapper.in("store_id", Arrays.asList(storeIds));
        }
        if (timeLine != null) {
            queryWrapper.eq("time_line", timeLine);
        }
        if (CharSequenceUtil.isNotEmpty(promotionApplyStatus)) {
            queryWrapper.eq("promotion_apply_status", PromotionsApplyStatusEnum.valueOf(promotionApplyStatus).name());
        }
        if (CharSequenceUtil.isNotEmpty(skuId)) {
            queryWrapper.eq("sku_id", skuId);
        }
        return queryWrapper;
    }

}
