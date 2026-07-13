package cn.lili.controller.seller.statistics;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
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
import java.util.Objects;

/**
 * 店铺端,流量统计接口
 *
 * @author Chopper
 * @since 2021/2/9 11:19
 */
@Tag(name = "店铺端,流量统计接口")
@RestController
@RequestMapping("/store/statistics/view")
public class ViewStatisticsStoreController {
    @Autowired
    private PlatformViewService platformViewService;

    @Operation(description = "流量数据 表单获取")
    @Parameter(name = "queryParam", description = "流量统计查询参数", required = true)
    @GetMapping("/list")
    public ResultMessage<List<PlatformViewVO>> getByPage(StatisticsQueryParam queryParam) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        queryParam.setStoreId(storeId);
        return ResultUtil.data(platformViewService.list(queryParam));
    }
}
