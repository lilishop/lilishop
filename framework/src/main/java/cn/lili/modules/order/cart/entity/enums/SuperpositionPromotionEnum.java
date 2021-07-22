package cn.lili.modules.order.cart.entity.enums;

/**
 * 活动叠加
 *
 * @author Chopper
 * @since 2020-03-25 2:30 下午
 */
public enum SuperpositionPromotionEnum {

    /**
     * 商品促销放在商品属性，这里只负责可叠加的其他促销
     * 叠加促销枚举，每一个商品，以下每个参数都只能参加一个
     */
    SELLER_COUPON("店铺优惠券"),
    PLATFORM_COUPON("平台优惠券"),
    FULL_DISCOUNT("满优惠");

    private final String description;

    SuperpositionPromotionEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
