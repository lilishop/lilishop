package cn.lili.controller.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.model.dto.GoodsStatisticsQueryParam;
import cn.lili.modules.statistics.model.vo.CategoryStatisticsDataVO;
import cn.lili.modules.statistics.model.vo.GoodsStatisticsDataVO;
import cn.lili.modules.statistics.service.GoodsStatisticsDataService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 管理端,商品统计接口
 *
 * @author Bulbasaur
 * @date: 2020/12/9 19:04
 */
@Api(tags = "管理端,商品统计接口")
@RestController
@RequestMapping("/manager/statistics/goods")
public class GoodsStatisticsManagerController {
    @Autowired
    private GoodsStatisticsDataService goodsStatisticsDataService;

    @ApiOperation(value = "获取统计列表,排行前一百的数据")
    @GetMapping
    public ResultMessage<List<GoodsStatisticsDataVO>> getByPage(GoodsStatisticsQueryParam goodsStatisticsQueryParam) {
        return ResultUtil.data(goodsStatisticsDataService.getGoodsStatisticsData(goodsStatisticsQueryParam, 100));
    }

    @ApiOperation(value = "获取行业统计列表")
    @GetMapping("/getCategoryByPage")
    public ResultMessage<List<CategoryStatisticsDataVO>> getCategoryByPage(GoodsStatisticsQueryParam goodsStatisticsQueryParam) {
        return ResultUtil.data(goodsStatisticsDataService.getCategoryStatisticsData(goodsStatisticsQueryParam));
    }
}
