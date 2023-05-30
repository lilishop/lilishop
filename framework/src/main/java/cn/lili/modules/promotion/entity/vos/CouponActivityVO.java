package cn.lili.modules.promotion.entity.vos;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.promotion.entity.dos.CouponActivity;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * 优惠券活动VO
 *
 * @author Bulbasaur
 * @since 2021/5/21 7:01 下午
 */
@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
public class CouponActivityVO extends CouponActivity {

    @ApiModelProperty(value = "优惠券活动下的优惠券列表")
    private List<CouponActivityItemVO> couponActivityItems;

    public CouponActivityVO(CouponActivity couponActivity, List<CouponActivityItemVO> couponActivityItemVOS) {
        BeanUtil.copyProperties(couponActivity, this);
        this.couponActivityItems = couponActivityItemVOS;
    }
}
