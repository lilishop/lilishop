package cn.lili.controller.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.model.dos.MemberStatisticsData;
import cn.lili.modules.statistics.model.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.service.MemberStatisticsDataService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 管理端,会员统计接口
 *
 * @author Bulbasaur
 * @date: 2020/12/9 19:04
 */
@Api(tags = "管理端,会员统计接口")
@RestController
@RequestMapping("/manager/statistics/member")
public class MemberStatisticsManagerController {
    @Autowired
    private MemberStatisticsDataService memberStatisticsDataService;

    @ApiOperation(value = "获取会员统计")
    @GetMapping
    public ResultMessage<List<MemberStatisticsData>> getByList(StatisticsQueryParam statisticsQueryParam) {
        return ResultUtil.data(memberStatisticsDataService.statistics(statisticsQueryParam));
    }
}
