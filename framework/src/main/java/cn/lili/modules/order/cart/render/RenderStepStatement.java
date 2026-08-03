package cn.lili.modules.order.cart.render;

import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;

/**
 * 价格渲染 步骤声明
 *
 * @author Chopper
 * @version v1.0
 * 2021-08-13 16:15
 */
public class RenderStepStatement {

    /**
     * 购物车购物车渲染
     * 校验商品 》 满优惠渲染  》  渲染优惠  》计算价格
     */
    public static RenderStepEnums[] cartRender = {
            RenderStepEnums.CHECK_DATA,
            RenderStepEnums.SKU_PROMOTION,
            RenderStepEnums.FULL_DISCOUNT,
            RenderStepEnums.NTH_ITEM_DISCOUNT,
            RenderStepEnums.CART_PRICE};

    /**
     * 结算页渲染
     * 过滤选择的商品 》 校验商品 》 满优惠渲染  》  渲染优惠  》
     * 优惠券渲染  》 计算运费  》  计算价格
     */
    public static RenderStepEnums[] checkedRender = {
            RenderStepEnums.CHECKED_FILTER,
            RenderStepEnums.CHECK_DATA,
            RenderStepEnums.SKU_PROMOTION,
            RenderStepEnums.FULL_DISCOUNT,
            RenderStepEnums.NTH_ITEM_DISCOUNT,
            RenderStepEnums.COUPON,
            RenderStepEnums.SKU_FREIGHT,
            RenderStepEnums.GIFT_CARD,
            RenderStepEnums.CART_PRICE,
    };


    /**
     * 单个商品优惠，不需要渲染满减优惠
     * 用于特殊场景：例如积分商品，拼团商品，虚拟商品等等
     */
    public static RenderStepEnums[] checkedSingleRender = {
            RenderStepEnums.CHECK_DATA,
            RenderStepEnums.SKU_PROMOTION,
            RenderStepEnums.COUPON,
            RenderStepEnums.SKU_FREIGHT,
            RenderStepEnums.GIFT_CARD,
            RenderStepEnums.CART_PRICE,
    };

    /**
     * 交易创建前渲染
     * 渲染购物车 生成SN 》分销人员佣金渲染 》平台佣金渲染
     */
    public static RenderStepEnums[] singleTradeRender = {
            RenderStepEnums.CHECK_DATA,
            RenderStepEnums.SKU_PROMOTION,
            RenderStepEnums.SKU_FREIGHT,
            RenderStepEnums.GIFT_CARD,
            RenderStepEnums.CART_PRICE,
            RenderStepEnums.CART_SN,
            RenderStepEnums.DISTRIBUTION,
            RenderStepEnums.PLATFORM_COMMISSION,
    };

    /**
     * 交易创建前渲染
     * 渲染购物车 生成SN 》分销人员佣金渲染 》平台佣金渲染
     */
    public static RenderStepEnums[] pintuanTradeRender = {
            RenderStepEnums.CHECK_DATA,
            RenderStepEnums.SKU_PROMOTION,
            RenderStepEnums.COUPON,
            RenderStepEnums.SKU_FREIGHT,
            RenderStepEnums.GIFT_CARD,
            RenderStepEnums.CART_PRICE,
            RenderStepEnums.CART_SN,
            RenderStepEnums.DISTRIBUTION,
            RenderStepEnums.PLATFORM_COMMISSION,

    };

    /**
     * 交易创建前渲染
     * 渲染购物车 生成SN 》分销人员佣金渲染 》平台佣金渲染
     */
    public static RenderStepEnums[] tradeRender = {
            RenderStepEnums.CHECKED_FILTER,
            RenderStepEnums.CHECK_DATA,
            RenderStepEnums.SKU_PROMOTION,
            RenderStepEnums.FULL_DISCOUNT,
            RenderStepEnums.NTH_ITEM_DISCOUNT,
            RenderStepEnums.COUPON,
            RenderStepEnums.SKU_FREIGHT,
            RenderStepEnums.GIFT_CARD,
            RenderStepEnums.CART_PRICE,
            RenderStepEnums.CART_SN,
            RenderStepEnums.DISTRIBUTION,
            RenderStepEnums.PLATFORM_COMMISSION,
    };

    /**
     * 电子卡券结算页渲染步骤：含 SKU_PROMOTION 写 basePrice，跳过满减/券/礼品卡/运费。
     */
    public static RenderStepEnums[] eCouponCheckedRender = {
            RenderStepEnums.CHECKED_FILTER,
            RenderStepEnums.CHECK_DATA,
            RenderStepEnums.SKU_PROMOTION,
            RenderStepEnums.CART_PRICE,
    };

    /**
     * 电子卡券建单前渲染步骤：在 checked 基础上增加 SN、分销、平台佣金等落库所需步骤。
     */
    public static RenderStepEnums[] eCouponTradeRender = {
            RenderStepEnums.CHECKED_FILTER,
            RenderStepEnums.CHECK_DATA,
            RenderStepEnums.SKU_PROMOTION,
            RenderStepEnums.CART_PRICE,
            RenderStepEnums.CART_SN,
            RenderStepEnums.DISTRIBUTION,
            RenderStepEnums.PLATFORM_COMMISSION,
    };
}
