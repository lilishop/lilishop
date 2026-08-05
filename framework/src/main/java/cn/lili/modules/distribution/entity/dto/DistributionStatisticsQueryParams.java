package cn.lili.modules.distribution.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 分销统计查询参数
 *
 * @author pikachu
 */
@Data
public class DistributionStatisticsQueryParams {

    @Schema(description = "开始时间")
    private Date startTime;

    @Schema(description = "结束时间")
    private Date endTime;

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "关键词")
    private String keyword;
}
