package cn.lili.controller.manager.live;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.live.entity.vos.LiveOrderStatisticsVO;
import cn.lili.modules.live.service.LiveOrderService;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Slf4j
@Validated
@RestController
@Tag(name = "直播订单接口")
@RequestMapping("/manager/live/order")
@RequiredArgsConstructor
public class LiveOrderManagerController {

    private final LiveOrderService liveOrderService;

    @Operation(summary = "查询直播订单 单行数据，方便列表展示")
    @GetMapping("/page")
    public ResultMessage<IPage<OrderSimpleVO>> queryLiveOrder(OrderSearchParams orderSearchParams) {
        return ResultUtil.data(liveOrderService.queryOrderPage(orderSearchParams));
    }

    @Operation(summary = "电商统计")
    @GetMapping("/statistics")
    public ResultMessage<LiveOrderStatisticsVO> statistics(String liveRoomId) {
        return ResultUtil.data(liveOrderService.statistics(liveRoomId));
    }
}
