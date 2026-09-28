package cn.lili.modules.appointment.support;

import cn.lili.modules.appointment.entity.dto.AppointmentCheckoutDTO;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 预约结算上下文暂存（创单链路 memberId → checkout 参数）
 *
 * @author Bulbasaur
 * @since 2026-09-18
 */
public final class AppointmentCheckoutContextHolder {

    private static final Map<String, AppointmentCheckoutDTO> CONTEXT = new ConcurrentHashMap<>();

    private AppointmentCheckoutContextHolder() {
    }

    /**
     * 绑定会员预约结算参数
     *
     * @param memberId 会员ID
     * @param checkout 结算参数
     */
    public static void bind(String memberId, AppointmentCheckoutDTO checkout) {
        if (memberId == null || checkout == null) {
            return;
        }
        CONTEXT.put(memberId, checkout);
    }

    /**
     * 读取会员预约结算参数
     *
     * @param memberId 会员ID
     * @return 结算参数
     */
    public static AppointmentCheckoutDTO get(String memberId) {
        return memberId == null ? null : CONTEXT.get(memberId);
    }

    /**
     * 移除会员预约结算参数
     *
     * @param memberId 会员ID
     * @return 被移除的结算参数
     */
    public static AppointmentCheckoutDTO remove(String memberId) {
        return memberId == null ? null : CONTEXT.remove(memberId);
    }
}
