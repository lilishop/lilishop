package cn.lili.controller.order;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.order.entity.dos.OrderComplaint;
import cn.lili.modules.order.order.entity.dto.OrderComplaintDTO;
import cn.lili.modules.order.order.entity.enums.CommunicationOwnerEnum;
import cn.lili.modules.order.order.entity.vo.OrderComplaintCommunicationVO;
import cn.lili.modules.order.order.entity.vo.OrderComplaintSearchParams;
import cn.lili.modules.order.order.entity.vo.OrderComplaintVO;
import cn.lili.modules.order.order.service.OrderComplaintCommunicationService;
import cn.lili.modules.order.order.service.OrderComplaintService;
import cn.lili.modules.order.order.service.OrderService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Objects;

/**
 * 买家端,交易投诉接口
 *
 * @author paulG
 * @since 2020/12/7
 **/
@RestController
@Tag(name = "买家端,交易投诉接口")
@RequestMapping("/buyer/order/complain")
public class OrderComplaintBuyerController {

    /**
     * 交易投诉
     */
    @Autowired
    private OrderComplaintService orderComplaintService;

    /**
     * 交易投诉沟通
     */
    @Autowired
    private OrderComplaintCommunicationService orderComplaintCommunicationService;

    @Autowired
    private OrderService orderService;


    @Operation(summary = "通过id获取")
    @Parameter(name = "id", description = "投诉单ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<OrderComplaintVO> get(@PathVariable String id) {
        OrderComplaintVO orderComplaintVO = OperationalJudgment.judgment(orderComplaintService.getOrderComplainById(id));
        return ResultUtil.data(orderComplaintVO);
    }

    @Operation(summary = "分页获取")
    @GetMapping
    public ResultMessage<IPage<OrderComplaint>> get(OrderComplaintSearchParams searchParams, PageVO pageVO) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        searchParams.setMemberId(currentUser.getId());
        return ResultUtil.data(orderComplaintService.getOrderComplainByPage(searchParams, pageVO));

    }

    @PreventDuplicateSubmissions
    @Operation(summary = "添加交易投诉")
    @PostMapping
    public ResultMessage<OrderComplaint> add(@Valid OrderComplaintDTO orderComplaintDTO) {
        OperationalJudgment.judgment(orderService.getBySn(orderComplaintDTO.getOrderSn()));
        return ResultUtil.data(orderComplaintService.addOrderComplain(orderComplaintDTO));
    }

    @Operation(summary = "添加交易投诉对话")
    @Parameter(name = "complainId", description = "投诉单ID", required = true)
    @Parameter(name = "content", description = "内容", required = true)
    @PostMapping("/communication")
    public ResultMessage<OrderComplaintCommunicationVO> addCommunication(@RequestParam String complainId, @RequestParam String content) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        // 安全校验：校验投诉单是否存在且属于当前买家，严防跨用户越权注入伪造投诉沟通信息
        OperationalJudgment.judgment(orderComplaintService.getOrderComplainById(complainId));
        OrderComplaintCommunicationVO communicationVO = new OrderComplaintCommunicationVO(complainId, content, CommunicationOwnerEnum.BUYER.name(), currentUser.getNickName(), currentUser.getId());
        orderComplaintCommunicationService.addCommunication(communicationVO);
        return ResultUtil.data(communicationVO);
    }


    @PreventDuplicateSubmissions
    @Operation(summary = "取消售后")
    @Parameter(name = "id", description = "投诉单ID", required = true)
    @PutMapping("/status/{id}")
    public ResultMessage<Object> cancel(@PathVariable String id) {
        orderComplaintService.cancel(id);
        return ResultUtil.success();
    }


}
