package cn.lili.modules.stock.event;

import cn.lili.modules.stock.entity.dto.StockFlowMessage;
import lombok.Getter;
import org.springframework.context.ApplicationEvent;

import java.util.Collections;
import java.util.List;

/**
 * 库存流水应用事件
 * 当库存扣减或增加入库成功后发布，供事务同步或消息桥接监听处理
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Getter
public class StockFlowEvent extends ApplicationEvent {

    private static final long serialVersionUID = 1L;

    /**
     * 待记录的库存流水消息列表
     */
    private final List<StockFlowMessage> messages;

    public StockFlowEvent(Object source, List<StockFlowMessage> messages) {
        super(source);
        this.messages = messages != null ? messages : Collections.emptyList();
    }

    public StockFlowEvent(Object source, StockFlowMessage message) {
        super(source);
        this.messages = message != null ? Collections.singletonList(message) : Collections.emptyList();
    }
}
