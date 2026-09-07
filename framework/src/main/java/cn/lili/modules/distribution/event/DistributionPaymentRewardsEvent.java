package cn.lili.modules.distribution.event;

import lombok.Getter;
import org.springframework.context.ApplicationEvent;

/**
 * 订单支付事务提交后触发分销入账。
 * <p>
 * 仅携带订单号，由独立门面在新事务中执行 {@code handlePaymentRewards}，
 * 避免分销写库失败把支付事务标成 rollback-only。
 * </p>
 *
 * @author Mike
 * @date 2026-09-07
 */
public class DistributionPaymentRewardsEvent extends ApplicationEvent {

    private static final long serialVersionUID = 1L;

    /**
     * 待入账订单号。
     */
    @Getter
    private final String orderSn;

    /**
     * 携带待入账订单号。
     *
     * @param source  事件源
     * @param orderSn 订单编号
     */
    public DistributionPaymentRewardsEvent(Object source, String orderSn) {
        super(source);
        this.orderSn = orderSn;
    }
}
