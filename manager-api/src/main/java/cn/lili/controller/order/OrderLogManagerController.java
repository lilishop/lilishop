package cn.lili.controller.order;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端,订单日志管理接口
 *
 * @author Chopper
 * @since 2020/11/17 4:34 下午
 */
@RestController
@Api(tags = "管理端,订单日志管理接口")
@RequestMapping("/manager/order/orderLog")
public class OrderLogManagerController {
    @Autowired
    private OrderLogService orderLogService;

    @GetMapping(value = "/get/{id}")
    @ApiOperation(value = "通过id获取")
    public ResultMessage<OrderLog> get(@PathVariable String id) {

        return ResultUtil.data(orderLogService.getById(id));
    }

    @GetMapping(value = "/getByPage")
    @ApiOperation(value = "分页获取")
    public ResultMessage<IPage<OrderLog>> getByPage(OrderLog entity,
                                                    SearchVO searchVo,
                                                    PageVO page) {
        return ResultUtil.data(orderLogService.page(PageUtil.initPage(page), PageUtil.initWrapper(entity, searchVo)));
    }

}
