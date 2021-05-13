package cn.lili.common.rocketmq.tags;

/**
 * @author paulG
 * @since 2020/12/9
 **/
public enum MemberTagsEnum {

    MEMBER_REGISTER("会员注册"),
    MEMBER_SING("会员签到"),
    MEMBER_WITHDRAWAL("会员提现"),
    MEMBER_POINT_CHANGE("会员积分变动");

    private final String description;

    MemberTagsEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }


}
