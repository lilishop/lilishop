package cn.lili.controller.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.BusinessCompositionDataVO;
import cn.lili.modules.statistics.entity.vo.OverViewDataVO;
import cn.lili.modules.statistics.entity.vo.SourceDataVO;
import cn.lili.modules.statistics.service.OverViewStatisticsService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 管理端,营业概览接口
 *
 * @author Bulbasaur
 * @since 2025/08/25 7:31 下午
 */
@Slf4j
@Api(tags = "管理端,营业概览接口")
@RestController
@RequestMapping("/manager/statistics/overview")
public class OverViewStatisticsManagerController {

    @Autowired
    private OverViewStatisticsService overViewStatisticsService;

    @ApiOperation(value = "获取营业概览统计")
    @GetMapping
    public ResultMessage<OverViewDataVO> overViewDataVO(StatisticsQueryParam statisticsQueryParam) {
        try {
            return ResultUtil.data(overViewStatisticsService.getOverViewDataVO(statisticsQueryParam));
        } catch (Exception e) {
            log.error("获取营业概览统计错误",e);
        }
        return null;
    }

    @ApiOperation(value = "收款构成列表")
    @GetMapping("/source")
    public ResultMessage<List<SourceDataVO>> sourceDataVOList(StatisticsQueryParam statisticsQueryParam) {
        try {
            return ResultUtil.data(overViewStatisticsService.getSourceDataVOList(statisticsQueryParam));
        } catch (Exception e) {
            log.error("收款构成列表错误",e);
        }
        return null;
    }

    @ApiOperation(value = "营业构成信息")
    @GetMapping("/businessComposition")
    public ResultMessage<BusinessCompositionDataVO> businessCompositionDataVO(StatisticsQueryParam statisticsQueryParam) {
        try {
            return ResultUtil.data(overViewStatisticsService.businessCompositionDataVO(statisticsQueryParam));
        } catch (Exception e) {
            log.error("营业构成信息",e);
        }
        return null;
    }
}
