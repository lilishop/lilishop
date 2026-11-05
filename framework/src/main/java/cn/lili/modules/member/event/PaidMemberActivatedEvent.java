package cn.lili.modules.member.event;

import lombok.Getter;
import org.springframework.context.ApplicationEvent;

/**
 * 付费会员订单首次确认到账事件，仅在支付事务成功提交后触发权益发放。
 * 仅传订单主键，监听端重新读取有效期和权益快照；事务回滚时不发放，重复到账不重复发布。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
public class PaidMemberActivatedEvent extends ApplicationEvent {
    /** 事件序列化版本。 */
    private static final long serialVersionUID = 1L;

    /** li_paid_member_order主键，不是会员购买业务订单号。 */
    @Getter
    private final String orderId;

    /**
     * 构造待支付事务提交后处理的资格事件。
     * @param source 事件发布源
     * @param orderId 付费会员订单主键
     */
    public PaidMemberActivatedEvent(Object source, String orderId) {
        super(source);
        this.orderId = orderId;
    }
}
