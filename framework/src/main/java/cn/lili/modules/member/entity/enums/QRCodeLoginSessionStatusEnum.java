package cn.lili.modules.member.entity.enums;


import lombok.Getter;

@Getter
public enum QRCodeLoginSessionStatusEnum {

    /**
     * 二维码创建完毕，等待app端扫码
     */
    WAIT_SCANNING(0,"等待扫码"),

    /**
     * app端已经扫码，等待确认同意登录
     */
    SCANNING(1,"已经扫码"),

    /**
     * 用户在app端点击了同意登录
     */
    VERIFIED(2,"确认登录"),

    /**
     * 用户在app端点击了取消登录
     */
    CANCELED(3,"取消登录"),

    /**
     * 二维码不存在/或者已经过期
     */
    NO_EXIST(4,"二维码已过期")

    ;


    private Integer code;

    private String desc;


    QRCodeLoginSessionStatusEnum(Integer code,String desc){
        this.code = code;
        this.desc = desc;
    }
}
