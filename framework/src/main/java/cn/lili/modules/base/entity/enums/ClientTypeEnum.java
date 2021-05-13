package cn.lili.modules.base.entity.enums;


/**
 * 客户端类型
 *
 * @author Chopper
 * @date 2020/12/8 9:46
 */

public enum ClientTypeEnum {

    H5("移动端"),
    PC("PC端"),
    WECHAT_MP("小程序端"),
    APP("移动应用端"),
    UNKNOWN("未知");

    private final String clientName;

    ClientTypeEnum(String des) {
        this.clientName = des;
    }

    public String clientName() {
        return this.clientName;
    }

    public String value() {
        return this.name();
    }
}
