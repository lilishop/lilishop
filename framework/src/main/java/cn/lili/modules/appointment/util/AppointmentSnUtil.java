package cn.lili.modules.appointment.util;

import cn.lili.common.utils.SnowFlake;

/**
 * 预约业务单号生成
 *
 * @author Bulbasaur
 * @since 2026-09-18
 */
public final class AppointmentSnUtil {

    /**
     * 预约业务前缀
     */
    public static final String PREFIX = "AP";

    private AppointmentSnUtil() {
    }

    /**
     * 生成工单编号
     *
     * @return 工单编号
     */
    public static String workOrderSn() {
        return SnowFlake.createStr(PREFIX);
    }

    /**
     * 生成核销码
     *
     * @param storeId 店铺ID
     * @return 8 位核销码
     */
    public static String verificationCode(String storeId) {
        long seed = Math.abs(SnowFlake.getId());
        if (storeId != null) {
            seed = Math.abs(seed + storeId.hashCode());
        }
        return String.format("%08d", seed % 100000000L);
    }
}
