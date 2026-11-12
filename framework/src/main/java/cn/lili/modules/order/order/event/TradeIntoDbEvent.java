package cn.lili.modules.order.order.event;

import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import lombok.Getter;
import org.springframework.context.ApplicationEvent;

/**
 * 交易单落库完成领域事件
 * <p>
 * 当购物车结算生成的主交易单及子订单成功持久化落库后发布，通知预约扩展表暂存等下游监听者。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Getter
public class TradeIntoDbEvent extends ApplicationEvent {

    private static final long serialVersionUID = 1L;

    private final TradeDTO tradeDTO;

    /**
     * 构造交易单落库完成领域事件
     *
     * @param source   事件源
     * @param tradeDTO 交易单数据传输对象
     */
    public TradeIntoDbEvent(Object source, TradeDTO tradeDTO) {
        super(source);
        this.tradeDTO = tradeDTO;
    }
}
