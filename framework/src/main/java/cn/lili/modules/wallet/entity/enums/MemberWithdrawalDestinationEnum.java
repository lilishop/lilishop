package cn.lili.modules.wallet.entity.enums;

/**
 * 会员提现到哪里 枚举
 *
 * @author Chopper
 * @since 2021/3/20 10:44
 */

public enum MemberWithdrawalDestinationEnum {
    /**
     * 提现目的地
     */
    WECHAT("微信账户"),
    WALLET("余额账户");

    private String description;

    MemberWithdrawalDestinationEnum(String str) {
        this.description = str;

    }

    public String description() {
        return description;
    }
}
