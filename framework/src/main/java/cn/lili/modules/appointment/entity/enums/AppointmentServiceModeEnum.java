package cn.lili.modules.appointment.entity.enums;

/**
 * 预约服务模式
 *
 * @author Bulbasaur
 * @since 2026-09-18
 */
public enum AppointmentServiceModeEnum {

    /**
     * 到店服务
     */
    IN_STORE("到店"),
    /**
     * 上门服务
     */
    ON_SITE("上门");

    private final String description;

    AppointmentServiceModeEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }
}
