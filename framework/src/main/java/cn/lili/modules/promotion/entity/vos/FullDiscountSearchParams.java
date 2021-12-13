package cn.lili.modules.promotion.entity.vos;

import cn.hutool.core.text.CharSequenceUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;
import java.util.Arrays;

/**
 * 满优惠查询通用类
 *
 * @author paulG
 * @since 2020/8/21
 **/
@EqualsAndHashCode(callSuper = true)
@Data
public class FullDiscountSearchParams extends BasePromotionsSearchParams implements Serializable {

    private static final long serialVersionUID = -4052716630253333681L;


    @ApiModelProperty(value = "活动名称")
    private String promotionName;

    @ApiModelProperty(value = "店铺编号 如有多个','分割")
    private String storeId;

    @ApiModelProperty(value = "是否赠优惠券")
    private Boolean isCoupon;

    @ApiModelProperty(value = "优惠券id")
    private String couponId;

    @Override
    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = super.queryWrapper();
        if (CharSequenceUtil.isNotEmpty(promotionName)) {
            queryWrapper.like("title", promotionName);
        }
        if (storeId != null) {
            queryWrapper.in("store_id", Arrays.asList(storeId.split(",")));
        }
        if (isCoupon != null) {
            queryWrapper.eq("is_coupon", isCoupon);
        }
        if (CharSequenceUtil.isNotEmpty(couponId)) {
            queryWrapper.eq("coupon_id", couponId);
        }
        return queryWrapper;
    }

}
