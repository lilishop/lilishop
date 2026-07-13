package cn.lili.controller.manager.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.vo.MemberDistributionVO;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.OnlineMemberVO;
import cn.lili.modules.statistics.entity.vo.PlatformViewVO;
import cn.lili.modules.statistics.service.PlatformViewService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 管理端,流量统计接口
 *
 * @author Chopper
 * @since 2021/2/9 11:19
 */
@Tag(name = "管理端,流量统计接口")
@RestController
@RequestMapping("/manager/statistics/view")
public class ViewStatisticsManagerController {
    @Autowired
    private PlatformViewService platformViewService;

    @Operation(summary = "流量数据 表单获取")
    @Parameter(name = "queryParam", description = "统计查询参数", required = true)
    @GetMapping("/list")
    public ResultMessage<List<PlatformViewVO>> getByPage(StatisticsQueryParam queryParam) {
        return ResultUtil.data(platformViewService.list(queryParam));
    }

    @Operation(summary = "当前在线人数")
    @GetMapping("/online/current")
    public ResultMessage<Long> currentNumberPeopleOnline() {
        return ResultUtil.data(platformViewService.online());
    }


    @Operation(summary = "客户分布")
    @GetMapping("/online/distribution")
    public ResultMessage<List<MemberDistributionVO>> memberDistribution() {
        return ResultUtil.data(platformViewService.memberDistribution());
    }

    @Operation(summary = "在线人数历史（默认48小时）")
    @GetMapping("/online/history")
    public ResultMessage<List<OnlineMemberVO>> history() {
        return ResultUtil.data(platformViewService.statisticsOnline());
    }

}
