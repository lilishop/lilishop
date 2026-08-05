package cn.lili.modules.distribution.entity.enums;

/**
 * 分销员上级归属模式
 *
 * @author pikachu
 */
public enum DistributionParentAttributionModeEnum {

    /** 本次招募邀请方 */
    RECRUIT_INVITER("招募邀请方"),

    /** 之前绑定的分销员 */
    BOUND_DISTRIBUTOR("绑定分销员");

    private final String description;

    DistributionParentAttributionModeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
