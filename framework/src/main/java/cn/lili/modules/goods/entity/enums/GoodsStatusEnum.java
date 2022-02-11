package cn.lili.modules.goods.entity.enums;

/**
 * 商品类型枚举
 *
 * @author pikachu
 * @since 2020-02-26 23:24:13
 */
public enum GoodsStatusEnum {
    /**
     * 上架
     */
    UPPER("上架"),
    /**
     * 下架
     */
    DOWN("下架");

    private final String description;

    GoodsStatusEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }
}
