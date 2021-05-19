package cn.lili.controller.statistics;

import cn.lili.common.security.context.UserContext;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.model.dto.GoodsStatisticsQueryParam;
import cn.lili.modules.statistics.model.vo.GoodsStatisticsDataVO;
import cn.lili.modules.statistics.model.vo.StoreIndexStatisticsVO;
import cn.lili.modules.statistics.service.GoodsStatisticsDataService;
import cn.lili.modules.statistics.service.IndexStatisticsService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 店铺端,首页统计接口
 *
 * @author Bulbasaur
 * @date: 2020/12/9 19:04
 */
@Api(tags = "店铺端,首页统计接口")
@RestController
@RequestMapping("/store/statistics/index")
public class IndexStatisticsStoreController {

    /**
     * 热卖商品统计
     */
    @Autowired
    private GoodsStatisticsDataService goodsStatisticsDataService;
    /**
     * 首页统计
     */
    @Autowired
    private IndexStatisticsService indexStatisticsService;

    @ApiOperation(value = "获取统计列表,排行前一百的数据")
    @GetMapping("/top100")
    public ResultMessage<List<GoodsStatisticsDataVO>> getByPage(GoodsStatisticsQueryParam statisticsQueryParam) {
        statisticsQueryParam.setStoreId(UserContext.getCurrentUser().getStoreId());
        return ResultUtil.data(goodsStatisticsDataService.getGoodsStatisticsData(statisticsQueryParam, 100));
    }

    @ApiOperation(value = "获取首页查询数据")
    @GetMapping
    public ResultMessage<StoreIndexStatisticsVO> index() {
        return ResultUtil.data(indexStatisticsService.storeIndexStatistics());
    }
}
