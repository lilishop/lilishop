package cn.lili.modules.distribution.entity.vos;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 分销员端客户成交统计
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionBuyerCustomerTradeStatsVO {

    @Schema(description = "订单数量（去重主订单号）")
    private Long orderCount;

    @Schema(description = "成交金额（实付汇总）")
    private BigDecimal tradeAmount;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "最后成交时间")
    private Date lastTradeTime;
}
