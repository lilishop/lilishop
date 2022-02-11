package cn.lili.modules.promotion.entity.vos;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * MemberCouponVO
 *
 * @author Chopper
 * @version v1.0
 * 2021-08-24 14:30
 */
@Data
public class MemberCouponVO extends MemberCoupon {

    @ApiModelProperty(value = "无法使用原因")
    private String reason;
    
    public MemberCouponVO(MemberCoupon memberCoupon, String reason) {
        BeanUtil.copyProperties(memberCoupon, this);
        this.reason = reason;
    }

    public MemberCouponVO(){

    }
}
