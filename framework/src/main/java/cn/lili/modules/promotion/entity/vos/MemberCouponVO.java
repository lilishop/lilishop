package cn.lili.modules.promotion.entity.vos;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * MemberCouponVO
 *
 * @author Chopper
 * @version v1.0
 * 2021-08-24 14:30
 */
@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
public class MemberCouponVO extends MemberCoupon {

    private static final long serialVersionUID = -5533168813075444962L;

    @ApiModelProperty(value = "优惠券名称")
    private String couponName;

    @ApiModelProperty(value = "无法使用原因")
    private String reason;
    
    public MemberCouponVO(MemberCoupon memberCoupon, String reason) {
        BeanUtil.copyProperties(memberCoupon, this);
        this.reason = reason;
    }

}
