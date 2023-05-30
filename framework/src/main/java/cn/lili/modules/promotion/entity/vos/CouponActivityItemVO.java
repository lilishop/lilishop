package cn.lili.modules.promotion.entity.vos;

import cn.lili.modules.promotion.entity.dos.CouponActivityItem;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 优惠券活动的优惠券VO
 *
 * @author Bulbasaur
 * @since 2021/6/18 11:00 上午
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class CouponActivityItemVO extends CouponActivityItem {

    @ApiModelProperty(value = "优惠券名称")
    private String couponName;

    @ApiModelProperty(value = "面额")
    private Double price;

    /**
     * POINT("打折"), PRICE("减免现金");
     *
     * @see cn.lili.modules.promotion.entity.enums.CouponTypeEnum
     */
    @ApiModelProperty(value = "优惠券类型")
    private String couponType;

    @ApiModelProperty(value = "折扣")
    private Double couponDiscount;
}
