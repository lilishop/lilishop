package cn.lili.modules.appointment.entity.enums;

import java.util.Arrays;
import java.util.List;

/**
 * 预约工单状态
 *
 * @author Bulbasaur
 * @since 2026-09-18
 */
public enum AppointmentWorkOrderStatusEnum {

    /**
     * 待领取
     */
    PENDING_CLAIM,
    /**
     * 已领取
     */
    CLAIMED,
    /**
     * 待核销
     */
    WAIT_VERIFY,
    /**
     * 服务中
     */
    IN_SERVICE,
    /**
     * 服务结束
     */
    SERVICE_ENDED,
    /**
     * 已退款
     */
    REFUNDED,
    /**
     * 逾期未到店
     */
    NO_SHOW;

    /**
     * 首页待办使用的未完成状态。服务结束和已退款不再计入。
     */
    public static List<String> unfinishedNames() {
        return Arrays.asList(
                PENDING_CLAIM.name(),
                CLAIMED.name(),
                WAIT_VERIFY.name(),
                IN_SERVICE.name(),
                NO_SHOW.name());
    }
}
