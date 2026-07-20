package cn.lili.controller.manager.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.MarketingOverviewVO;
import cn.lili.modules.statistics.service.MarketingStatisticsService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端,营销概况接口
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Tag(name = "管理端,营销概况接口")
@RestController
@RequestMapping("/manager/statistics/marketing")
public class MarketingStatisticsManagerController {

    @Autowired
    private MarketingStatisticsService marketingStatisticsService;

    @Operation(summary = "营销概况")
    @Parameter(name = "statisticsQueryParam", description = "统计查询参数")
    @GetMapping
    public ResultMessage<MarketingOverviewVO> overview(StatisticsQueryParam statisticsQueryParam) {
        return ResultUtil.data(marketingStatisticsService.overview(statisticsQueryParam));
    }
}
