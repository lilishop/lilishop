package cn.lili.modules.distribution.entity.enums;

/**
 * 分销员状态
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
public enum DistributionStatusEnum {
    /**
     * 申请中
     */
    APPLY("申请中"),
    /**
     * 已清退
     */
    RETREAT("已清退"),
    /**
     * 审核拒绝
     */
    REFUSE("审核拒绝"),
    /**
     * 审核通过
     */
    PASS("审核通过");

    private final String description;

    DistributionStatusEnum(String description) {
        this.description = description;
    }
}
