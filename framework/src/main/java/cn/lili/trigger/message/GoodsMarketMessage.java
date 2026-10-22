package cn.lili.trigger.message;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * 商品上下架延时任务消息
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class GoodsMarketMessage {

    @Schema(description = "商品ID列表")
    private List<String> goodsIds;

    @Schema(description = "原因")
    private String reason;

    @Schema(description = "触发时间（毫秒时间戳，到秒），用于执行时校验是否为最新计划")
    private Long triggerTime;

    public GoodsMarketMessage(List<String> goodsIds, String reason) {
        this.goodsIds = goodsIds;
        this.reason = reason;
    }
}

