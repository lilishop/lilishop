package cn.lili.rocketmq.tags;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
public enum StoreTagsEnum {

    EDIT_STORE_SETTING("修改商家设置");

    private final String description;

    StoreTagsEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }
}
