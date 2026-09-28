package cn.lili.modules.distribution.event;

import lombok.Getter;
import org.springframework.context.ApplicationEvent;

/**
 * 订单支付事务提交后尝试分销招募自动申请。
 *
 * @author Mike
 * @date 2026-09-18
 */
public class DistributionRecruitAutoApplyEvent extends ApplicationEvent {

    private static final long serialVersionUID = 1L;

    /**
     * 下单会员 ID。
     */
    @Getter
    private final String memberId;

    /**
     * 携带待自动申请的会员 ID。
     *
     * @param source   事件源
     * @param memberId 会员 ID
     */
    public DistributionRecruitAutoApplyEvent(Object source, String memberId) {
        super(source);
        this.memberId = memberId;
    }
}
