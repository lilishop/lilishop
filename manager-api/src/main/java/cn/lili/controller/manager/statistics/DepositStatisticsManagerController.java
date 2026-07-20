package cn.lili.controller.manager.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.entity.vo.DepositBucketVO;
import cn.lili.modules.statistics.service.DepositStatisticsService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 管理端,储值分析接口
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Tag(name = "管理端,储值分析接口")
@RestController
@RequestMapping("/manager/statistics/deposit")
public class DepositStatisticsManagerController {

    @Autowired
    private DepositStatisticsService depositStatisticsService;

    @Operation(summary = "余额分布")
    @GetMapping("/balanceDistribution")
    public ResultMessage<List<DepositBucketVO>> balanceDistribution() {
        return ResultUtil.data(depositStatisticsService.balanceDistribution());
    }

    @Operation(summary = "充值次数分布")
    @GetMapping("/rechargeTimesDistribution")
    public ResultMessage<List<DepositBucketVO>> rechargeTimesDistribution() {
        return ResultUtil.data(depositStatisticsService.rechargeTimesDistribution());
    }

    @Operation(summary = "充值金额分布")
    @GetMapping("/rechargeAmountDistribution")
    public ResultMessage<List<DepositBucketVO>> rechargeAmountDistribution() {
        return ResultUtil.data(depositStatisticsService.rechargeAmountDistribution());
    }
}
