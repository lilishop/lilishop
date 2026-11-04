package cn.lili.modules.promotion.entity.vos;

import cn.lili.modules.promotion.entity.dos.CouponActivityItem;
import io.swagger.v3.oas.annotations.media.Schema;
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

    @Schema(description = "优惠券名称")
    private String couponName;

    @Schema(description = "面额")
    private Double price;

    /**
     * POINT("打折"), PRICE("减免现金");
     *
     * @see cn.lili.modules.promotion.entity.enums.CouponTypeEnum
     */
    @Schema(description = "优惠券类型")
    private String couponType;

    @Schema(description = "消费门槛，0表示无门槛")
    private Double consumeThreshold;

    @Schema(description = "折扣")
    private Double couponDiscount;
}
