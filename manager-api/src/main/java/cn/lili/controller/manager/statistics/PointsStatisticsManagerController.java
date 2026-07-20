package cn.lili.controller.manager.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.entity.vo.PointsAnalysisVO;
import cn.lili.modules.statistics.entity.vo.PointsDistributionVO;
import cn.lili.modules.statistics.entity.vo.PointsIdentityStatVO;
import cn.lili.modules.statistics.entity.vo.PointsSourceDistributionVO;
import cn.lili.modules.statistics.service.PointsStatisticsService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 管理端,积分分析接口
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Tag(name = "管理端,积分分析接口")
@RestController
@RequestMapping("/manager/statistics/points")
public class PointsStatisticsManagerController {

    @Autowired
    private PointsStatisticsService pointsStatisticsService;

    @Operation(summary = "积分统计概览")
    @GetMapping
    public ResultMessage<PointsAnalysisVO> overview() {
        return ResultUtil.data(pointsStatisticsService.overview());
    }

    @Operation(summary = "客户可用积分分布")
    @GetMapping("/distribution")
    public ResultMessage<List<PointsDistributionVO>> distribution() {
        return ResultUtil.data(pointsStatisticsService.distribution());
    }

    @Operation(summary = "积分累计分发分布")
    @GetMapping("/sourceDistribution")
    public ResultMessage<List<PointsSourceDistributionVO>> sourceDistribution() {
        return ResultUtil.data(pointsStatisticsService.sourceDistribution());
    }

    @Operation(summary = "客户身份积分累计统计")
    @GetMapping("/identityStat")
    public ResultMessage<List<PointsIdentityStatVO>> identityStat() {
        return ResultUtil.data(pointsStatisticsService.identityStat());
    }
}
