package cn.lili.modules.appointment.entity.enums;

/**
 * 预约核销码状态
 *
 * @author Bulbasaur
 * @since 2026-09-18
 */
public enum AppointmentVerificationCodeStatusEnum {

    /**
     * 待核销
     */
    UNUSED,
    /**
     * 已核销
     */
    USED,
    /**
     * 已作废
     */
    VOIDED
}
