package cn.lili.modules.statistics.entity.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 交易趋势按日统计数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class TradeTrendVO {

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @Schema(description = "日期")
    private Date date;

    @Schema(description = "下单笔数")
    private Long orderNum = 0L;

    @Schema(description = "下单金额")
    private Double orderAmount = 0D;

    @Schema(description = "付款订单数")
    private Long paymentOrderNum = 0L;

    @Schema(description = "付款金额")
    private Double paymentAmount = 0D;

    @Schema(description = "退款笔数")
    private Long refundOrderNum = 0L;

    @Schema(description = "退款金额")
    private Double refundOrderPrice = 0D;
}
