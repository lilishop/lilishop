package cn.lili.controller.manager.order;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.order.entity.dos.OrderComplaint;
import cn.lili.modules.order.order.entity.enums.CommunicationOwnerEnum;
import cn.lili.modules.order.order.entity.enums.OrderComplaintStatusEnum;
import cn.lili.modules.order.order.entity.vo.OrderComplaintCommunicationVO;
import cn.lili.modules.order.order.entity.vo.OrderComplaintOperationParams;
import cn.lili.modules.order.order.entity.vo.OrderComplaintSearchParams;
import cn.lili.modules.order.order.entity.vo.OrderComplaintVO;
import cn.lili.modules.order.order.service.OrderComplaintCommunicationService;
import cn.lili.modules.order.order.service.OrderComplaintService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Objects;

/**
 * 管理端,交易投诉接口
 *
 * @author paulG
 * @since 2020/12/5
 */
@RestController
@Tag(name = "管理端,交易投诉接口")
@RequestMapping("/manager/order/complain")
public class OrderComplaintManagerController {

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

    @Operation(description = "通过id获取")
    @Parameter(name = "id", description = "投诉单ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<OrderComplaintVO> get(@PathVariable String id) {
        return ResultUtil.data(orderComplaintService.getOrderComplainById(id));
    }

    @Operation(description = "分页获取")
    @Parameter(name = "searchParams", description = "查询参数")
    @Parameter(name = "pageVO", description = "分页参数")
    @GetMapping
    public ResultMessage<IPage<OrderComplaint>> get(OrderComplaintSearchParams searchParams, PageVO pageVO) {
        return ResultUtil.data(orderComplaintService.getOrderComplainByPage(searchParams, pageVO));
    }

    @Operation(description = "更新数据")
    @Parameter(name = "orderComplainVO", description = "投诉单")
    @PutMapping
    public ResultMessage<OrderComplaintVO> update(OrderComplaintVO orderComplainVO) {
        orderComplaintService.updateOrderComplain(orderComplainVO);
        return ResultUtil.data(orderComplainVO);

    }

    @Operation(description = "添加交易投诉对话")
    @Parameter(name = "complainId", description = "投诉单ID", required = true)
    @Parameter(name = "content", description = "内容", required = true)
    @PostMapping("/communication")
    public ResultMessage<OrderComplaintCommunicationVO> addCommunication(@RequestParam String complainId, @RequestParam String content) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        OrderComplaintCommunicationVO communicationVO = new OrderComplaintCommunicationVO(complainId, content, CommunicationOwnerEnum.PLATFORM.name(), currentUser.getUsername(), currentUser.getId());
        orderComplaintCommunicationService.addCommunication(communicationVO);
        return ResultUtil.data(communicationVO);
    }

    @PreventDuplicateSubmissions
    @Operation(description = "修改状态")
    @Parameter(name = "orderComplainVO", description = "投诉单")
    @PutMapping("/status")
    public ResultMessage<Object> updateStatus(OrderComplaintOperationParams orderComplainVO) {
        orderComplaintService.updateOrderComplainByStatus(orderComplainVO);
        return ResultUtil.success();
    }


    @PreventDuplicateSubmissions
    @Operation(description = "仲裁")
    @Parameter(name = "id", description = "投诉单ID", required = true)
    @Parameter(name = "arbitrationResult", description = "仲裁结果", required = true)
    @PutMapping("/complete/{id}")
    public ResultMessage<Object> complete(@PathVariable String id, String arbitrationResult) {
        //新建对象
        OrderComplaintOperationParams orderComplaintOperationParams = new OrderComplaintOperationParams();
        orderComplaintOperationParams.setComplainId(id);
        orderComplaintOperationParams.setArbitrationResult(arbitrationResult);
        orderComplaintOperationParams.setComplainStatus(OrderComplaintStatusEnum.COMPLETE.name());

        //修改状态
        orderComplaintService.updateOrderComplainByStatus(orderComplaintOperationParams);
        return ResultUtil.success();
    }
}
