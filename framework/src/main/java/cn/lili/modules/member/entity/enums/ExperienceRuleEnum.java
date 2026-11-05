package cn.lili.modules.member.entity.enums;

/**
 * 经验值规则场景
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
public enum ExperienceRuleEnum {
    CONSUME("消费"),
    REGISTER("注册"),
    SIGN_IN("签到"),
    COMMENT("评价"),
    SHARE("分享商城"),
    PROFILE("完善信息"),
    FOLLOW_STORE("关注店铺"),
    BIND_WECHAT("绑定微信"),
    ADD_ADDRESS("添加收货地址"),
    SHARE_REGISTER("分享注册"),
    SHARE_BUY("分享购买");

    private final String description;

    ExperienceRuleEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }
}
