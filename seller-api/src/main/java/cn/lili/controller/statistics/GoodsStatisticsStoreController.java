package cn.lili.controller.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.entity.dto.GoodsStatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.GoodsStatisticsDataVO;
import cn.lili.modules.statistics.service.StoreFlowStatisticsService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Objects;

/**
 * 店铺端,商品统计接口
 *
 * @author Bulbasaur
 * @since 2020/11/22 14:23
 */
@Api(tags = "店铺端,商品统计接口")
@RestController
@RequestMapping("/store/statistics/goods")
public class GoodsStatisticsStoreController {

    /**
     * 商品统计
     */
    @Autowired
    private StoreFlowStatisticsService storeFlowStatisticsService;

    @ApiOperation(value = "获取统计列表,排行前一百的数据")
    @GetMapping
    public ResultMessage<List<GoodsStatisticsDataVO>> getByPage(GoodsStatisticsQueryParam statisticsQueryParam) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        statisticsQueryParam.setStoreId(storeId);
        return ResultUtil.data(storeFlowStatisticsService.getGoodsStatisticsData(statisticsQueryParam, 100));
    }
}
