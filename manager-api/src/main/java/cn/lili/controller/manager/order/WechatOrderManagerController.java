package cn.lili.controller.manager.order;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.vo.OrderNumVO;
import cn.lili.modules.wechat.entity.vo.WechatOrderVO;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.wechat.entity.dos.WechatOrderSyncLog;
import cn.lili.modules.wechat.entity.dto.WechatOrderSearchParams;
import cn.lili.modules.wechat.service.WechatMPService;
import cn.lili.modules.wechat.service.WechatOrderSyncJournal;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 平台小程序微信支付订单的状态查询、补报和日志入口；沿用订单查询/编辑权限，不自动扩大授权。
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
@RestController
@RequestMapping("/manager/order/order/wechat")
@Tag(name = "管理端,小程序订单同步")
public class WechatOrderManagerController {
    @Autowired
    private OrderService orderService;
    @Autowired
    private WechatOrderSyncJournal journal;
    @Autowired
    private WechatMPService wechatService;

    /** 复用订单分页，在服务端强制小程序微信支付范围并批量回填上报状态。 */
    @GetMapping
    @Operation(summary = "小程序订单及上报状态分页")
    public ResultMessage<IPage<WechatOrderVO>> page(WechatOrderSearchParams params) {
        journal.requireManager(UserContext.getCurrentUser());
        return ResultUtil.data(journal.fill(orderService.queryManageByParams(params)));
    }

    /** 状态数量复用相同查询对象，保证支付范围及上报筛选与列表一致。 */
    @GetMapping("/orderNum")
    @Operation(summary = "小程序订单数量")
    public ResultMessage<OrderNumVO> counts(WechatOrderSearchParams params) {
        journal.requireManager(UserContext.getCurrentUser());
        return ResultUtil.data(orderService.getOrderNumVO(params));
    }

    /** 人工单笔补报；方法权限由管理端既有订单编辑权限控制，身份和履约规则在服务层再校验。 */
    @DemoSite
    @PreventDuplicateSubmissions
    @PostMapping("/{orderSn}/retry")
    @Operation(summary = "单笔补报微信发货信息")
    public ResultMessage<String> retry(@PathVariable String orderSn) {
        return ResultUtil.data(wechatService.retryShippingInfo(orderSn));
    }

    /** 按支付流水分页查询全部关联子订单的同步日志，禁止跨渠道订单借此查看。 */
    @GetMapping("/{orderSn}/logs")
    @Operation(summary = "微信订单同步日志")
    public ResultMessage<IPage<WechatOrderSyncLog>> logs(@PathVariable String orderSn,
                                                       @RequestParam(defaultValue = "1") int pageNumber,
                                                       @RequestParam(defaultValue = "20") int pageSize) {
        journal.requireManager(UserContext.getCurrentUser());
        Order order = orderService.getBySn(orderSn);
        if (order == null || !"WECHAT_MP".equals(order.getClientType()) || !"WECHAT".equals(order.getPaymentMethod())) {
            throw new ServiceException("订单不存在或不属于小程序微信支付范围");
        }
        return ResultUtil.data(journal.logs(order, pageNumber, pageSize));
    }
}
