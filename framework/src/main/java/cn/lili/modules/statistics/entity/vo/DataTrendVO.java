package cn.lili.modules.statistics.entity.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 数据概况按日趋势数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class DataTrendVO {

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @Schema(description = "日期")
    private Date date;

    @Schema(description = "营业额/付款金额")
    private Double turnover = 0D;

    @Schema(description = "订单数")
    private Long orderNum = 0L;

    @Schema(description = "新增客户数")
    private Long memberNum = 0L;
}
