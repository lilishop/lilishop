package cn.lili.modules.promotion.entity.enums;


/**
 * 促销分类枚举
 *
 * @author Chopper
 * @date 2021/2/1 19:32
 */
public enum PromotionTypeEnum {
    /**
     * 促销枚举
     */
    PINTUAN("拼团"),
    SECKILL("秒杀"),
    COUPON("优惠券"),
    FULL_DISCOUNT("满减"),
    POINTS_GOODS("积分商品"),
    COUPON_ACTIVITY("优惠券活动")
    ;

    /**
     * 拼团秒杀拥有独立库存，如果其他促销也有独立库存涉及库存扣减的，请添加在下方
     */
    static PromotionTypeEnum[] haveStockPromotion = new PromotionTypeEnum[]{PINTUAN, SECKILL};

    private final String description;

    PromotionTypeEnum(String description) {
        this.description = description;
    }

    /**
     * 是否拥有库存
     */
    public static boolean haveStock(String promotionType) {
        for (PromotionTypeEnum promotionTypeEnum : haveStockPromotion) {
            if (promotionTypeEnum.name().equals(promotionType)) {
                return true;
            }
        }
        return false;
    }

    public String description() {
        return description;
    }

}
