package cn.lili.rocketmq.tags;

/**
 * 会员操作枚举
 *
 * @author paulG
 * @since 2020/12/9
 **/
public enum MemberTagsEnum {
    /**
     * 会员注册
     */
    MEMBER_REGISTER("会员注册"),
    /**
     * 会员注册
     */
    MEMBER_LOGIN("会员登录"),
    /**
     * 会员签到
     */
    MEMBER_SING("会员签到"),
    /**
     * 会员提现
     */
    MEMBER_WITHDRAWAL("会员提现"),
    /**
     * 会员积分变动
     */
    MEMBER_POINT_CHANGE("会员积分变动");

    private final String description;

    MemberTagsEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }


}
