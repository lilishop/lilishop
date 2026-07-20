package cn.lili.controller.manager.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.DistributionOverviewVO;
import cn.lili.modules.statistics.entity.vo.DistributionRankVO;
import cn.lili.modules.statistics.service.DistributionStatisticsService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 管理端,分销统计接口
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Tag(name = "管理端,分销统计接口")
@RestController
@RequestMapping("/manager/statistics/distribution")
public class DistributionStatisticsManagerController {

    @Autowired
    private DistributionStatisticsService distributionStatisticsService;

    @Operation(summary = "分销概况")
    @Parameter(name = "statisticsQueryParam", description = "统计查询参数")
    @GetMapping
    public ResultMessage<DistributionOverviewVO> overview(StatisticsQueryParam statisticsQueryParam) {
        return ResultUtil.data(distributionStatisticsService.overview(statisticsQueryParam));
    }

    @Operation(summary = "TOP分销员")
    @GetMapping("/rank/distributor")
    public ResultMessage<List<DistributionRankVO>> topDistributors(StatisticsQueryParam statisticsQueryParam) {
        return ResultUtil.data(distributionStatisticsService.topDistributors(statisticsQueryParam));
    }

    @Operation(summary = "TOP分销商品")
    @GetMapping("/rank/goods")
    public ResultMessage<List<DistributionRankVO>> topGoods(StatisticsQueryParam statisticsQueryParam) {
        return ResultUtil.data(distributionStatisticsService.topGoods(statisticsQueryParam));
    }
}
