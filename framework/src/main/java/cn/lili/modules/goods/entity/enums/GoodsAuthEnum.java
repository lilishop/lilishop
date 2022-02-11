package cn.lili.modules.goods.entity.enums;

/**
 * 商品审核
 *
 * @author pikachu
 * @since 2020-02-26 23:24:13
 */
public enum GoodsAuthEnum {
    /**
     * 需要审核 并且待审核
     */
    TOBEAUDITED("待审核"),
    /**
     * 审核通过
     */
    PASS("审核通过"),
    /**
     * 审核通过
     */
    REFUSE("审核拒绝");

    private final String description;

    GoodsAuthEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }
}
