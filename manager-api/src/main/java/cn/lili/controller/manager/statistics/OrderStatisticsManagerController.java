package cn.lili.controller.manager.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.OrderOverviewVO;
import cn.lili.modules.statistics.entity.vo.OrderStatisticsDataVO;
import cn.lili.modules.statistics.entity.vo.TradeTrendVO;
import cn.lili.modules.statistics.service.AfterSaleStatisticsService;
import cn.lili.modules.statistics.service.OrderStatisticsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 管理端,订单统计接口
 *
 * @author Bulbasaur
 * @since 2020/12/9 19:04
 */
@Slf4j
@Tag(name = "管理端,订单统计接口")
@RestController
@RequestMapping("/manager/statistics/order")
public class OrderStatisticsManagerController {
    @Autowired
    private OrderStatisticsService orderStatisticsService;
    @Autowired
    private AfterSaleStatisticsService afterSaleStatisticsService;

    @Operation(summary = "订单概览统计")
    @Parameter(name = "statisticsQueryParam", description = "统计查询参数", required = true)
    @GetMapping("/overview")
    public ResultMessage<OrderOverviewVO> overview(StatisticsQueryParam statisticsQueryParam) {
        try {
            return ResultUtil.data(orderStatisticsService.overview(statisticsQueryParam));
        } catch (Exception e) {
            log.error("订单概览统计错误",e);
        }
        return null;
    }

    @Operation(summary = "订单图表统计")
    @Parameter(name = "statisticsQueryParam", description = "统计查询参数", required = true)
    @GetMapping
    public ResultMessage<List<OrderStatisticsDataVO>> statisticsChart(StatisticsQueryParam statisticsQueryParam) {
        try {
            return ResultUtil.data(orderStatisticsService.statisticsChart(statisticsQueryParam));
        } catch (Exception e) {
            log.error("订单图表统计",e);
        }
        return null;
    }


    @Operation(summary = "订单统计")
    @Parameter(name = "statisticsQueryParam", description = "统计查询参数", required = true)
    @Parameter(name = "pageVO", description = "分页参数", required = true)
    @GetMapping("/order")
    public ResultMessage<IPage<OrderSimpleVO>> order(StatisticsQueryParam statisticsQueryParam, PageVO pageVO) {
        try {
            return ResultUtil.data(orderStatisticsService.getStatistics(statisticsQueryParam, pageVO));
        } catch (Exception e) {
            log.error("订单统计",e);
        }
        return null;
    }


    @Operation(summary = "退单统计")
    @Parameter(name = "statisticsQueryParam", description = "统计查询参数", required = true)
    @Parameter(name = "pageVO", description = "分页参数", required = true)
    @GetMapping("/refund")
    public ResultMessage<IPage<AfterSale>> refund(StatisticsQueryParam statisticsQueryParam, PageVO pageVO) {
        return ResultUtil.data(afterSaleStatisticsService.getStatistics(statisticsQueryParam, pageVO));
    }

    @Operation(summary = "交易趋势按日")
    @Parameter(name = "statisticsQueryParam", description = "统计查询参数", required = true)
    @GetMapping("/tradeTrend")
    public ResultMessage<List<TradeTrendVO>> tradeTrend(StatisticsQueryParam statisticsQueryParam) {
        return ResultUtil.data(orderStatisticsService.tradeTrend(statisticsQueryParam));
    }
}
