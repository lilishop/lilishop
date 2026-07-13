package cn.lili.controller.manager.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.RefundOrderStatisticsDataVO;
import cn.lili.modules.statistics.service.RefundOrderStatisticsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "管理端,退款统计接口")
@RestController
@RequestMapping("/manager/statistics/refundOrder")
public class RefundOrderStatisticsManagerController {
    @Autowired
    private RefundOrderStatisticsService refundOrderStatisticsService;

    @Operation(summary = "获取退款统计列表")
    @Parameter(name = "pageVO", description = "分页参数", required = true)
    @Parameter(name = "statisticsQueryParam", description = "统计查询参数", required = true)
    @GetMapping("/getByPage")
    public ResultMessage<IPage<RefundOrderStatisticsDataVO>> getByPage(PageVO pageVO, StatisticsQueryParam statisticsQueryParam) {
        return ResultUtil.data(refundOrderStatisticsService.getRefundOrderStatisticsData(pageVO, statisticsQueryParam));
    }

    @Operation(summary = "获取退款统计金额")
    @Parameter(name = "statisticsQueryParam", description = "统计查询参数", required = true)
    @GetMapping("/getPrice")
    public ResultMessage<Object> getPrice(StatisticsQueryParam statisticsQueryParam) {
        return ResultUtil.data(refundOrderStatisticsService.getRefundOrderStatisticsPrice(statisticsQueryParam));
    }
}
