package cn.lili.modules.distribution.entity.enums;

/**
 * 分销客户锁客模式
 *
 * @author pikachu
 */
public enum DistributionBindModeEnum {

    /** 永久锁客 */
    PERMANENT("永久锁客"),

    /** 短期锁客 */
    SHORT_TERM("短期锁客");

    private final String description;

    DistributionBindModeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
