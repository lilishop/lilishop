package cn.lili.modules.distribution.entity.enums;

/**
 * 分销员自购直接佣金归属模式
 *
 * @author pikachu
 */
public enum DistributionSelfPurchaseModeEnum {

    /** 佣金给分销员自己 */
    SELF("佣金给分销员自己"),

    /** 佣金给其有效客户关系绑定方 */
    BOUND_DISTRIBUTOR("佣金给绑定分销员"),

    /** 无直接佣金 */
    NONE("无直接佣金");

    private final String description;

    DistributionSelfPurchaseModeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
