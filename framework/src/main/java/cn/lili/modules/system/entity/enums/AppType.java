package cn.lili.modules.system.entity.enums;


/**
 * app类型 安卓 IOS
 *
 * @author Chopper
 * @since 2020/9/11 17:03
 */
public enum AppType {

    /**
     * IOS
     */
    IOS("IOS"),
    /**
     * 安卓
     */
    ANDROID("安卓");

    private final String description;

    AppType(String description) {
        this.description = description;

    }

    public String getDescription() {
        return description;
    }

    public String description() {
        return this.description;
    }

}

