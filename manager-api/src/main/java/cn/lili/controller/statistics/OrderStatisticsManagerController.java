package cn.lili.controller.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.order.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.order.order.service.AfterSaleService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.statistics.model.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.model.vo.OrderOverviewVO;
import cn.lili.modules.statistics.model.vo.OrderStatisticsDataVO;
import cn.lili.modules.statistics.service.OrderStatisticsDataService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
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
 * @date: 2020/12/9 19:04
 */
@Slf4j
@Api(tags = "管理端,订单统计接口")
@RestController
@RequestMapping("/manager/statistics/order")
public class OrderStatisticsManagerController {
    @Autowired
    private OrderStatisticsDataService orderStatisticsDataService;
    @Autowired
    private OrderService orderService;
    @Autowired
    private AfterSaleService afterSaleService;

    @ApiOperation(value = "订单概览统计")
    @GetMapping("/overview")
    public ResultMessage<OrderOverviewVO> overview(StatisticsQueryParam statisticsQueryParam) {
        try {
            return ResultUtil.data(orderStatisticsDataService.overview(statisticsQueryParam));
        } catch (Exception e) {
            log.error("订单概览统计错误",e);
        }
        return null;
    }

    @ApiOperation(value = "订单图表统计")
    @GetMapping
    public ResultMessage<List<OrderStatisticsDataVO>> statisticsChart(StatisticsQueryParam statisticsQueryParam) {
        try {
            return ResultUtil.data(orderStatisticsDataService.statisticsChart(statisticsQueryParam));
        } catch (Exception e) {
            log.error("订单图表统计",e);
        }
        return null;
    }


    @ApiOperation(value = "订单统计")
    @GetMapping("/order")
    public ResultMessage<IPage<OrderSimpleVO>> order(StatisticsQueryParam statisticsQueryParam, PageVO pageVO) {
        try {
            return ResultUtil.data(orderService.getStatistics(statisticsQueryParam, pageVO));
        } catch (Exception e) {
            log.error("订单统计",e);
        }
        return null;
    }


    @ApiOperation(value = "退单统计")
    @GetMapping("/refund")
    public ResultMessage<IPage<AfterSale>> refund(StatisticsQueryParam statisticsQueryParam, PageVO pageVO) {
        return ResultUtil.data(afterSaleService.getStatistics(statisticsQueryParam, pageVO));
    }
}
