package cn.lili.modules.distribution.entity.enums;

/**
 * 分销商品审核状态
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public enum DistributionGoodsAuthEnum {

    /** 待审核 */
    TOBEAUDITED("待审核"),

    /** 已审核 */
    PASS("已审核"),

    /** 审核未通过 */
    REFUSE("审核未通过");

    private final String description;

    DistributionGoodsAuthEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }
}
