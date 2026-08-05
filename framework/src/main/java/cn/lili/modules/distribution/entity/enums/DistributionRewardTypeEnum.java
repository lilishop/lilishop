package cn.lili.modules.distribution.entity.enums;

/**
 * 分销奖励类型
 *
 * @author pikachu
 */
public enum DistributionRewardTypeEnum {

    /** 直属佣金 */
    DIRECT("直属佣金"),

    /** 邀请奖励 */
    INVITE("邀请奖励");

    private final String description;

    DistributionRewardTypeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
