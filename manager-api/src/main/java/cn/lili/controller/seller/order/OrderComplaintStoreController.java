package cn.lili.controller.seller.order;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.order.entity.dos.OrderComplaint;
import cn.lili.modules.order.order.entity.enums.CommunicationOwnerEnum;
import cn.lili.modules.order.order.entity.vo.*;
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
 * 店铺端,交易投诉接口
 *
 * @author paulG
 * @since 2020/12/5
 **/
@RestController
@Tag(name = "店铺端,交易投诉接口")
@RequestMapping("/store/order/complain")
public class OrderComplaintStoreController {

    /**
     * 交易投诉
     */
    @Autowired
    private OrderComplaintService orderComplaintService;

    /**
     * 投诉沟通
     */
    @Autowired
    private OrderComplaintCommunicationService orderComplaintCommunicationService;

    @Operation(summary = "通过id获取")
    @Parameter(name = "id", description = "投诉单ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<OrderComplaintVO> get(@PathVariable String id) {
        return ResultUtil.data(OperationalJudgment.judgment(orderComplaintService.getOrderComplainById(id)));
    }

    @Operation(summary = "分页获取")
    @Parameter(name = "searchParams", description = "投诉查询参数")
    @Parameter(name = "pageVO", description = "分页参数")
    @GetMapping
    public ResultMessage<IPage<OrderComplaint>> get(OrderComplaintSearchParams searchParams, PageVO pageVO) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        searchParams.setStoreId(storeId);
        return ResultUtil.data(orderComplaintService.getOrderComplainByPage(searchParams, pageVO));
    }



    @Operation(summary = "添加交易投诉对话")
    @Parameter(name = "complainId", description = "投诉单ID", required = true)
    @Parameter(name = "content", description = "内容", required = true)
    @PostMapping("/communication")
    public ResultMessage<OrderComplaintCommunicationVO> addCommunication(@RequestParam String complainId, @RequestParam String content) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        OrderComplaintCommunicationVO communicationVO = new OrderComplaintCommunicationVO(complainId, content, CommunicationOwnerEnum.STORE.name(), currentUser.getUsername(), currentUser.getStoreId());
        orderComplaintCommunicationService.addCommunication(communicationVO);
        return ResultUtil.success();
    }

    @Operation(summary = "修改申诉信息")
    @Parameter(name = "orderComplainVO", description = "投诉信息")
    @PutMapping
    public ResultMessage<OrderComplaintVO> update(OrderComplaintVO orderComplainVO) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        orderComplainVO.setStoreId(storeId);
        orderComplaintService.updateOrderComplain(orderComplainVO);
        return ResultUtil.data(orderComplainVO);
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "申诉")
    @Parameter(name = "storeAppealVO", description = "申诉信息")
    @PutMapping("/appeal")
    public ResultMessage<OrderComplaintVO> appeal(StoreAppealVO storeAppealVO) {
        orderComplaintService.appeal(storeAppealVO);
        return ResultUtil.data(orderComplaintService.getOrderComplainById(storeAppealVO.getOrderComplaintId()));
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "修改状态")
    @Parameter(name = "orderComplainVO", description = "投诉状态参数")
    @PutMapping("/status")
    public ResultMessage<Object> updateStatus(OrderComplaintOperationParams orderComplainVO) {
        orderComplaintService.updateOrderComplainByStatus(orderComplainVO);
        return ResultUtil.success();
    }

}
