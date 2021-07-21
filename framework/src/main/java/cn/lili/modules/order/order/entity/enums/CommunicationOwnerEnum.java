package cn.lili.modules.order.order.entity.enums;

/**
 * 投诉角色枚举
 *
 * @author paulG
 * @since 2020/12/7
 **/
public enum CommunicationOwnerEnum {

    /**
     * 买家
     */
    BUYER("买家"),

    /**
     * 卖家
     */
    STORE("卖家"),

    /**
     * 平台
     */
    PLATFORM("平台");

    private final String description;

    CommunicationOwnerEnum(String description) {
        this.description = description;
    }

    public String description() {
        return this.description;
    }

}
