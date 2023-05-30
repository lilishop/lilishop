package cn.lili.modules.wallet.entity.enums;

/**
 * 提现申请状态枚举类
 *
 * @author pikachu
 * @since 2020-11-06
 */
public enum WithdrawStatusEnum {
    /**
     * 申请中
     */
    APPLY("申请中"),
    /**
     * 审核成功即提现成功
     */
    VIA_AUDITING("审核通过"),
    /**
     * 审核未通过
     */
    FAIL_AUDITING("审核未通过"),
    /**
     * 提现成功
     */
    SUCCESS("提现成功"),
    /**
     * 提现失败
     */
    ERROR("提现失败");

    private String description;

    public String description() {
        return description;
    }

    WithdrawStatusEnum(String description) {
        this.description = description;
    }

}
