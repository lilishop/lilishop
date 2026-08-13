package cn.lili.modules.order.order.entity.enums;

/**
 * 商家发货履约模式
 *
 * @author Mike
 * @since 2026-08-13
 */
public enum FulfillmentModeEnum {

    /** 整单快递发货 */
    WHOLE,

    /** 拆单发货 */
    SPLIT,

    /** 无需配送（商家自配/同城/线下交付等，以备注说明） */
    NO_DELIVERY;

    public static FulfillmentModeEnum of(String name) {
        if (name == null || name.isBlank()) {
            return null;
        }
        return FulfillmentModeEnum.valueOf(name.trim());
    }
}
