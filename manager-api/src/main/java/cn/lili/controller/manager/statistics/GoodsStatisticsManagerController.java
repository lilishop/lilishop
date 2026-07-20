package cn.lili.controller.manager.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.entity.dto.GoodsStatisticsQueryParam;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.CategoryStatisticsDataVO;
import cn.lili.modules.statistics.entity.vo.GoodsOverviewVO;
import cn.lili.modules.statistics.entity.vo.GoodsRankVO;
import cn.lili.modules.statistics.entity.vo.GoodsStatisticsDataVO;
import cn.lili.modules.statistics.service.GoodsOverviewStatisticsService;
import cn.lili.modules.statistics.service.StoreFlowStatisticsService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 管理端,商品统计接口
 *
 * @author Bulbasaur
 * @since 2020/12/9 19:04
 */
@Tag(name = "管理端,商品流水统计接口")
@RestController
@RequestMapping("/manager/statistics/goods")
public class GoodsStatisticsManagerController {
    @Autowired
    private StoreFlowStatisticsService storeFlowStatisticsService;
    @Autowired
    private GoodsOverviewStatisticsService goodsOverviewStatisticsService;

    @Operation(summary = "获取统计列表,排行前一百的数据")
    @Parameter(name = "goodsStatisticsQueryParam", description = "商品统计查询参数", required = true)
    @GetMapping
    public ResultMessage<List<GoodsStatisticsDataVO>> getByPage(GoodsStatisticsQueryParam goodsStatisticsQueryParam) {
        return ResultUtil.data(storeFlowStatisticsService.getGoodsStatisticsData(goodsStatisticsQueryParam, 100));
    }

    @Operation(summary = "获取行业统计列表")
    @Parameter(name = "goodsStatisticsQueryParam", description = "商品统计查询参数", required = true)
    @GetMapping("/getCategoryByPage")
    public ResultMessage<List<CategoryStatisticsDataVO>> getCategoryByPage(GoodsStatisticsQueryParam goodsStatisticsQueryParam) {
        return ResultUtil.data(storeFlowStatisticsService.getCategoryStatisticsData(goodsStatisticsQueryParam));
    }

    @Operation(summary = "商品概况")
    @GetMapping("/overview")
    public ResultMessage<GoodsOverviewVO> overview(StatisticsQueryParam statisticsQueryParam) {
        return ResultUtil.data(goodsOverviewStatisticsService.overview(statisticsQueryParam));
    }

    @Operation(summary = "退货排行榜TOP10")
    @GetMapping("/rank/refund")
    public ResultMessage<List<GoodsRankVO>> refundRank(StatisticsQueryParam statisticsQueryParam) {
        return ResultUtil.data(goodsOverviewStatisticsService.refundRank(statisticsQueryParam));
    }

    @Operation(summary = "畅销排行榜TOP10")
    @GetMapping("/rank/sales")
    public ResultMessage<List<GoodsRankVO>> salesRank(StatisticsQueryParam statisticsQueryParam) {
        return ResultUtil.data(goodsOverviewStatisticsService.salesRank(statisticsQueryParam));
    }
}
