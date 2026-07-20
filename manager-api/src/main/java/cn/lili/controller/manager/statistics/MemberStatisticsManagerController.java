package cn.lili.controller.manager.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.entity.dos.MemberStatisticsData;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.MemberAnalysisVO;
import cn.lili.modules.statistics.entity.vo.MemberGenderDistributionVO;
import cn.lili.modules.statistics.entity.vo.MemberNewTrendVO;
import cn.lili.modules.statistics.entity.vo.MemberOverviewVO;
import cn.lili.modules.statistics.entity.vo.MemberRegionDistributionVO;
import cn.lili.modules.statistics.service.MemberOverviewStatisticsService;
import cn.lili.modules.statistics.service.MemberStatisticsService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 管理端,客户统计接口
 *
 * @author Bulbasaur
 * @since 2020/12/9 19:04
 */
@Tag(name = "管理端,客户统计接口")
@RestController
@RequestMapping("/manager/statistics/member")
public class MemberStatisticsManagerController {
    @Autowired
    private MemberStatisticsService memberStatisticsService;
    @Autowired
    private MemberOverviewStatisticsService memberOverviewStatisticsService;

    @Operation(summary = "获取客户统计")
    @Parameter(name = "statisticsQueryParam", description = "统计查询参数", required = true)
    @GetMapping
    public ResultMessage<List<MemberStatisticsData>> getByList(StatisticsQueryParam statisticsQueryParam) {
        return ResultUtil.data(memberStatisticsService.statistics(statisticsQueryParam));
    }

    @Operation(summary = "会员概况")
    @GetMapping("/overview")
    public ResultMessage<MemberOverviewVO> overview(StatisticsQueryParam param) {
        return ResultUtil.data(memberOverviewStatisticsService.overview(param));
    }

    @Operation(summary = "会员新增人数趋势")
    @GetMapping("/trend")
    public ResultMessage<List<MemberNewTrendVO>> newMemberTrend(StatisticsQueryParam param) {
        return ResultUtil.data(memberOverviewStatisticsService.newMemberTrend(param));
    }

    @Operation(summary = "客户分析")
    @GetMapping("/analysis")
    public ResultMessage<MemberAnalysisVO> analysis(StatisticsQueryParam param) {
        return ResultUtil.data(memberOverviewStatisticsService.analysis(param));
    }

    @Operation(summary = "性别分布")
    @GetMapping("/distribution/gender")
    public ResultMessage<List<MemberGenderDistributionVO>> genderDistribution() {
        return ResultUtil.data(memberOverviewStatisticsService.genderDistribution());
    }

    @Operation(summary = "地域分布")
    @GetMapping("/distribution/region")
    public ResultMessage<List<MemberRegionDistributionVO>> regionDistribution() {
        return ResultUtil.data(memberOverviewStatisticsService.regionDistribution());
    }
}
