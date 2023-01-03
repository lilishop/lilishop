package cn.lili.controller.orders;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.im.entity.dto.ImQueryParams;
import cn.lili.modules.member.entity.dto.FootPrintQueryParams;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * IM端,订单接口
 *
 * @author chc
 * @since 2022/6/2114:46
 */
@Slf4j
@Api(tags = "IM端,订单接口")
@RestController
@RequestMapping("/im/orders/orders")
public class ImOrderComtroller {

    @Autowired
    private OrderService orderService;

    @GetMapping("")
    @ApiOperation(value = "获取会员订单列表")
    public ResultMessage<IPage<OrderSimpleVO>> getMemberHistory(OrderSearchParams params) {
        return ResultUtil.data(orderService.queryByParams(params));
    }
}
