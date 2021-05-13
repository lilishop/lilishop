package cn.lili.modules.distribution.entity.enums;

/**
 * 分销佣金状态
 *
 * @author pikachu
 * @date 2020-03-14 23:04:56
 */
public enum DistributionCashStatusEnum {
    /**
     * 待处理
     */
    APPLY("待处理"),
    /**
     * 通过
     */
    PASS("通过"),
    /**
     * 拒绝
     */
    REFUSE("拒绝");


    private final String description;

    DistributionCashStatusEnum(String description) {
        this.description = description;
    }
}
