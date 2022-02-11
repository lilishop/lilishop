package cn.lili.modules.member.entity.enums;

/**
 * 发送类型
 *
 * @author Chopper
 * @since 2020-03-10 11:35 上午
 */
public enum SendTypeEnum {

    /**
     * 消息类型
     */
    ALL("全部"),
    SELECT("指定会员");

    private String description;

    SendTypeEnum(String str) {
        this.description = str;

    }

    public String description() {
        return description;
    }

}
