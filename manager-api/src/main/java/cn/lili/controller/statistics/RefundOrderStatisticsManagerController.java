package cn.lili.controller.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.RefundOrderStatisticsDataVO;
import cn.lili.modules.statistics.service.RefundOrderStatisticsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端,退款统计接口
 *
 * @author Bulbasaur
 * @since 2020/12/9 19:04
 */
@Api(tags = "管理端,退款统计接口")
@RestController
@RequestMapping("/manager/statistics/refundOrder")
public class RefundOrderStatisticsManagerController {
    @Autowired
    private RefundOrderStatisticsService refundOrderStatisticsService;

    @ApiOperation(value = "获取退款统计列表")
    @GetMapping("/getByPage")
    public ResultMessage<IPage<RefundOrderStatisticsDataVO>> getByPage(PageVO pageVO, StatisticsQueryParam statisticsQueryParam) {
        return ResultUtil.data(refundOrderStatisticsService.getRefundOrderStatisticsData(pageVO, statisticsQueryParam));
    }

    @ApiOperation(value = "获取退款统计金额")
    @GetMapping("/getPrice")
    public ResultMessage<Object> getPrice(StatisticsQueryParam statisticsQueryParam) {
        return ResultUtil.data(refundOrderStatisticsService.getRefundOrderStatisticsPrice(statisticsQueryParam));
    }
}
