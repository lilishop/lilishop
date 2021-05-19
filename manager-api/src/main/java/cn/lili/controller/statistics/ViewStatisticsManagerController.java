package cn.lili.controller.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.vo.MemberDistributionVO;
import cn.lili.modules.statistics.model.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.model.vo.OnlineMemberVO;
import cn.lili.modules.statistics.model.vo.PlatformViewVO;
import cn.lili.modules.statistics.service.PlatformViewDataService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 管理端,流量统计接口
 *
 * @author Chopper
 * @date 2021/2/9 11:19
 */
@Api(tags = "管理端,流量统计接口")
@RestController
@RequestMapping("/manager/statistics/view")
public class ViewStatisticsManagerController {
    @Autowired
    private PlatformViewDataService platformViewDataService;

    @ApiOperation(value = "流量数据 表单获取")
    @GetMapping("/list")
    public ResultMessage<List<PlatformViewVO>> getByPage(StatisticsQueryParam queryParam) {
        return ResultUtil.data(platformViewDataService.list(queryParam));
    }

    @ApiOperation(value = "当前在线人数")
    @GetMapping("/online/current")
    public ResultMessage<Long> currentNumberPeopleOnline() {
        return ResultUtil.data(platformViewDataService.online());
    }


    @ApiOperation(value = "会员分布")
    @GetMapping("/online/distribution")
    public ResultMessage<List<MemberDistributionVO>> memberDistribution() {
        return ResultUtil.data(platformViewDataService.memberDistribution());
    }

    @ApiOperation(value = "在线人数历史（默认48小时）")
    @GetMapping("/online/history")
    public ResultMessage<List<OnlineMemberVO>> history() {
        return ResultUtil.data(platformViewDataService.statisticsOnline());
    }

}
