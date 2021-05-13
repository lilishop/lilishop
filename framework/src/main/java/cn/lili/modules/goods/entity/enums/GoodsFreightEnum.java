package cn.lili.modules.goods.entity.enums;

/**
 * 商品运费承担者
 *
 * @author pikachu
 * @date 2020-02-26 23:24:13
 */
public enum GoodsFreightEnum {
    /**
     * 买家承担运费
     */
    BUYER("买家承担运费"),
    /**
     * 卖家承担运费
     */
    STORE("卖家承担运费");

    private final String description;

    GoodsFreightEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }
}
