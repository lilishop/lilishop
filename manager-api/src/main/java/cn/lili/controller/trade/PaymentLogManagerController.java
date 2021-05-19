package cn.lili.controller.trade;

import cn.lili.common.utils.PageUtil;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.vo.PaymentLog;
import cn.lili.modules.payment.service.PaymentService;
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
 * @date 2020/11/17 4:34 下午
 */
@RestController
@Api(tags = "管理端,收款日志接口")
@RequestMapping("/manager/paymentLog")
@Transactional(rollbackFor = Exception.class)
public class PaymentLogManagerController {

    @Autowired
    private PaymentService paymentService;


    @GetMapping
    @ApiOperation(value = "分页获取支付日志")
    public ResultMessage<IPage<PaymentLog>> getByPage(Order order,
                                                      SearchVO searchVo,
                                                      PageVO page) {
        return ResultUtil.data(paymentService.page(PageUtil.initPage(page), PageUtil.initWrapper(order, searchVo)));
    }
}
