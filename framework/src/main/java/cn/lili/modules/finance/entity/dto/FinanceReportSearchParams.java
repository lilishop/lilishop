package cn.lili.modules.finance.entity.dto;

import cn.lili.common.vo.PageVO;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 财务报表查询参数。
 * <p>
 * 时间支持两种形式：{@code startTime/endTime}（精确到秒）或 {@code startDate/endDate}（日期字符串）。
 * 均未传时由 Service 层默认取近 30 天。
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class FinanceReportSearchParams extends PageVO {

    /** 按日聚合 */
    public static final String GRANULARITY_DAY = "DAY";

    /** 按月聚合 */
    public static final String GRANULARITY_MONTH = "MONTH";

    @Schema(description = "店铺ID")
    private String storeId;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "开始时间")
    private Date startTime;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "结束时间")
    private Date endTime;

    @Schema(description = "起始日期")
    private String startDate;

    @Schema(description = "结束日期")
    private String endDate;

    @Schema(description = "粒度 DAY|MONTH")
    private String granularity = GRANULARITY_DAY;
}
