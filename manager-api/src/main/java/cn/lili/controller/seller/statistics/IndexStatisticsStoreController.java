package cn.lili.controller.seller.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.entity.dto.GoodsStatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.GoodsStatisticsDataVO;
import cn.lili.modules.statistics.entity.vo.StoreIndexStatisticsVO;
import cn.lili.modules.statistics.service.IndexStatisticsService;
import cn.lili.modules.statistics.service.StoreFlowStatisticsService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Objects;

/**
 * 店铺端,首页统计接口
 *
 * @author Bulbasaur
 * @since 2020/12/9 19:04
 */
@Tag(name = "店铺端,首页统计接口")
@RestController
@RequestMapping("/store/statistics/index")
public class IndexStatisticsStoreController {

    /**
     * 热卖商品统计
     */
    @Autowired
    private StoreFlowStatisticsService storeFlowStatisticsService;
    /**
     * 首页统计
     */
    @Autowired
    private IndexStatisticsService indexStatisticsService;

    @Operation(description = "获取统计列表,排行前一百的数据")
    @Parameter(name = "statisticsQueryParam", description = "商品统计查询参数", required = true)
    @GetMapping("/top100")
    public ResultMessage<List<GoodsStatisticsDataVO>> getByPage(GoodsStatisticsQueryParam statisticsQueryParam) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        statisticsQueryParam.setStoreId(storeId);
        return ResultUtil.data(storeFlowStatisticsService.getGoodsStatisticsData(statisticsQueryParam, 100));
    }

    @Operation(description = "获取首页查询数据")
    @GetMapping
    public ResultMessage<StoreIndexStatisticsVO> index() {
        return ResultUtil.data(indexStatisticsService.storeIndexStatistics());
    }
}
