package cn.lili.modules.order.order.entity.enums;

import cn.lili.common.utils.StringUtils;

import java.util.Arrays;
import java.util.EnumSet;

/**
 * 订单促销类型枚举
 *
 * @author Chopper
 * @since 2020/11/17 7:28 下午
 */
public enum OrderPromotionTypeEnum {

    /**
     * 普通订单
     */
    NORMAL,
    /**
     * 赠品订单
     */
    GIFT,
    /**
     * 拼团订单
     */
    PINTUAN,
    /**
     * 积分订单
     */
    POINTS,
    /**
     * 砍价订单
     */
    KANJIA;

    /**
     * 判断促销类型是否有效
     * @param typeEnumValue
     * @return
     */
    public static boolean isValid(String typeEnumValue) {
        if (StringUtils.isBlank(typeEnumValue)) {
            return false;
        }
        return Arrays.stream(OrderPromotionTypeEnum.values()).anyMatch(c -> c.name().equals(typeEnumValue));
    }

    /**
     * 判断订单类型是否可售后
     * GIFT\POINTS\KANJIA 三种促销类型的订单不可进行售后
     * @return true 可售后 false 不可售后
     */
    public static boolean isCanAfterSale(String orderPromotionType) {
        if (!isValid(orderPromotionType)) {
            return true;
        }
        EnumSet<OrderPromotionTypeEnum> noAfterSale = EnumSet.of(OrderPromotionTypeEnum.GIFT, OrderPromotionTypeEnum.POINTS, OrderPromotionTypeEnum.KANJIA);
        return !noAfterSale.contains(OrderPromotionTypeEnum.valueOf(orderPromotionType));
    }
}
