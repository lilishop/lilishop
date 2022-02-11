package cn.lili.common.enums;

/**
 * 开关枚举
 * @author Chopper
 */
public enum SwitchEnum {

    /**
     * 开关
     */
    OPEN("开启"), CLOSE("关闭");

    private String description;

    SwitchEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }


}
