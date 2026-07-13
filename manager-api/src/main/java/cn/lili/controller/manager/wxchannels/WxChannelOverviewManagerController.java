package cn.lili.controller.manager.wxchannels;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.wxchannels.entity.vo.WxChannelOverviewDailyVO;
import cn.lili.modules.wxchannels.entity.vo.WxChannelOverviewSummaryVO;
import cn.lili.modules.wxchannels.service.WxChannelOverviewService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@Tag(name = "管理端,微信视频号概况接口")
@RequestMapping("/manager/wxchannels/overview")
public class WxChannelOverviewManagerController {

    @Autowired
    private WxChannelOverviewService overviewService;

    @GetMapping("/summary")
    @Operation(summary = "视频号销售概括")
    @Parameter(name = "startTime", description = "开始时间")
    @Parameter(name = "endTime", description = "结束时间")
    public ResultMessage<WxChannelOverviewSummaryVO> summary(Long startTime, Long endTime) {
        return ResultUtil.data(overviewService.summary(startTime, endTime));
    }

    @GetMapping("/daily")
    @Operation(summary = "视频号日报")
    @Parameter(name = "startTime", description = "开始时间")
    @Parameter(name = "endTime", description = "结束时间")
    public ResultMessage<List<WxChannelOverviewDailyVO>> daily(Long startTime, Long endTime) {
        return ResultUtil.data(overviewService.daily(startTime, endTime));
    }

    @GetMapping("/export")
    @Operation(summary = "报表下载")
    @Parameter(name = "startTime", description = "开始时间")
    @Parameter(name = "endTime", description = "结束时间")
    public void export(HttpServletResponse response, Long startTime, Long endTime) {
        overviewService.export(response, startTime, endTime);
    }
}
