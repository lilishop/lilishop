package cn.lili.modules.verification.entity.enums;


/**
 * 验证码资源枚举
 *
 * @author Chopper
 * @since 2021/1/26 15:55
 */
public enum VerificationSourceEnum {
    /**
     * 滑块
     */
    SLIDER("滑块"),
    /**
     * 验证码源
     */
    RESOURCE("验证码源");

    private final String description;

    VerificationSourceEnum(String des) {
        this.description = des;
    }
}
