package cn.lili.modules.distribution.entity.enums;

/**
 * 分销商品默认排序
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public enum DistributionGoodsDefaultSortEnum {

    /** 商品佣金越高越靠前 */
    HIGHEST_COMMISSION,

    /** 商品价格越高越靠前 */
    HIGHEST_PRICE,

    /** 商品销量越高越靠前 */
    HIGHEST_SALES,

    /** 商品上架越晚越靠前 */
    LATEST_LISTING
}
