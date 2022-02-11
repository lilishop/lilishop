package cn.lili.controller.order;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.vo.PaymentLog;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * 管理端,收款日志接口
 *
 * @author Chopper
 * @since 2020/11/17 4:34 下午
 */
@RestController
@Api(tags = "管理端,收款日志接口")
@RequestMapping("/manager/paymentLog")
public class PaymentLogManagerController {

    @Autowired
    private OrderService orderService;


    @GetMapping
    @ApiOperation(value = "分页获取支付日志")
    public ResultMessage<IPage<PaymentLog>> getByPage(Order order,
                                                      SearchVO searchVo,
                                                      PageVO page) {
        return ResultUtil.data(orderService.queryPaymentLogs(PageUtil.initPage(page), PageUtil.initWrapper(order, searchVo)));
    }
}
