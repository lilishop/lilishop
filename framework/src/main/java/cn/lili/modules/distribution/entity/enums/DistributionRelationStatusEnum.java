package cn.lili.modules.distribution.entity.enums;

/**
 * 分销客户关系状态
 *
 * @author pikachu
 */
public enum DistributionRelationStatusEnum {

    /** 待支付转永久 */
    PENDING("待支付转永久"),

    /** 短期有效 */
    TEMPORARY("短期有效"),

    /** 永久有效 */
    PERMANENT("永久有效"),

    /** 已到期 */
    EXPIRED("已到期"),

    /** 已解绑 */
    UNBOUND("已解绑"),

    /** 自绑定 */
    SELF_BIND("自绑定");

    private final String description;

    DistributionRelationStatusEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
