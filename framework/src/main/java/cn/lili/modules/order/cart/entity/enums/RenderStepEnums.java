package cn.lili.modules.order.cart.entity.enums;

/**
 * 购物车渲染枚举
 */
public enum RenderStepEnums {

    /**
     * 购物车渲染枚举
     */
    CHECK_DATA("校验商品"),
    CHECKED_FILTER("选择商品过滤"),
    COUPON("优惠券价格渲染"),
    SKU_PROMOTION("商品促销计算"),
    FULL_DISCOUNT("满减计算"),
    SKU_FREIGHT("运费计算"),
    DISTRIBUTION("分配需要分配的促销金额"),
    PLATFORM_COMMISSION("平台佣金"),
    CART_PRICE("购物车金额计算"),
    CART_SN("交易编号创建");

    private String distribution;

    public String getDistribution() {
        return distribution;
    }

    RenderStepEnums(String distribution) {
        this.distribution = distribution;
    }
}
