package cn.lili.modules.distribution.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 买家端业绩统计查询参数
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionBuyerPerformanceQueryDTO {

    /**
     * 时间范围类型：ALL / TODAY / YESTERDAY / LAST_7_DAYS / CUSTOM
     */
    @Schema(description = "时间范围类型")
    private String rangeType = "ALL";

    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "自定义开始时间")
    private Date startTime;

    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "自定义结束时间")
    private Date endTime;
}
