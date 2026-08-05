package cn.lili.modules.distribution.entity.enums;

/**
 * 分销钱包流水类型
 *
 * @author pikachu
 */
public enum DistributionWalletLogTypeEnum {

    /** 佣金冻结 */
    COMMISSION_FREEZE("佣金冻结"),

    /** 佣金结算 */
    COMMISSION_SETTLE("佣金结算"),

    /** 佣金退款 */
    COMMISSION_REFUND("佣金退款"),

    /** 提现申请 */
    WITHDRAW_APPLY("提现申请"),

    /** 提现审核通过 */
    WITHDRAW_APPROVE("提现审核通过"),

    /** 提现审核拒绝 */
    WITHDRAW_REJECT("提现审核拒绝"),

    /** 人工调账 */
    MANUAL_ADJUST("人工调账");

    private final String description;

    DistributionWalletLogTypeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
