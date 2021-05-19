package cn.lili.controller.statistics;

import cn.lili.common.security.context.UserContext;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.model.dto.StatisticsQueryParam;
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
 * 店铺端,流量统计接口
 *
 * @author Chopper
 * @date 2021/2/9 11:19
 */
@Api(tags = "店铺端,流量统计接口")
@RestController
@RequestMapping("/store/statistics/view")
public class ViewStatisticsStoreController {
    @Autowired
    private PlatformViewDataService platformViewDataService;

    @ApiOperation(value = "流量数据 表单获取")
    @GetMapping("/list")
    public ResultMessage<List<PlatformViewVO>> getByPage(StatisticsQueryParam queryParam) {
        queryParam.setStoreId(UserContext.getCurrentUser().getStoreId());
        return ResultUtil.data(platformViewDataService.list(queryParam));
    }
}
