package cn.lili.controller.manager.appointment;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.appointment.entity.dto.AppointmentNoShowRefundDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentPlatformRefundDTO;
import cn.lili.modules.appointment.entity.dto.search.AppointmentWorkOrderSearchParams;
import cn.lili.modules.appointment.entity.vo.AppointmentBuyerOrderDetailVO;
import cn.lili.modules.appointment.entity.vo.AppointmentWorkOrderVO;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.system.entity.dto.AppointmentOrderSetting;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 运营端预约工单接口
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@RestController
@Tag(name = "管理端,预约工单")
@RequestMapping("/manager/appointment")
public class AppointmentManagerController {

    @Autowired
    private AppointmentService appointmentService;

    @Operation(summary = "预约订单扩展（时段与补充信息）")
    @GetMapping("/orders/{orderSn}")
    public ResultMessage<AppointmentBuyerOrderDetailVO> getOrderExtDetail(@PathVariable String orderSn) {
        return ResultUtil.data(appointmentService.getOrderExtDetail(orderSn));
    }

    @Operation(summary = "全平台工单分页")
    @GetMapping("/work-orders")
    public ResultMessage<IPage<AppointmentWorkOrderVO>> pageWorkOrders(AppointmentWorkOrderSearchParams params) {
        return ResultUtil.data(appointmentService.pageWorkOrders(params, true));
    }

    @Operation(summary = "平台预约退款")
    @PostMapping("/orders/{orderSn}/platform-refund")
    public ResultMessage<Object> platformRefund(@PathVariable String orderSn,
                                                @Valid @RequestBody AppointmentPlatformRefundDTO dto) {
        appointmentService.platformRefund(orderSn, dto);
        return ResultUtil.success();
    }

    @Operation(summary = "上门爽约退款")
    @PostMapping("/orders/{orderSn}/no-show-refund")
    public ResultMessage<Object> noShowRefund(@PathVariable String orderSn,
                                              @Valid @RequestBody AppointmentNoShowRefundDTO dto) {
        appointmentService.noShowRefund(orderSn, dto);
        return ResultUtil.success();
    }

    @Operation(summary = "预约订单设置")
    @GetMapping("/settings")
    public ResultMessage<AppointmentOrderSetting> getSettings() {
        return ResultUtil.data(appointmentService.getAppointmentOrderSetting());
    }

    @Operation(summary = "保存预约订单设置")
    @PutMapping("/settings")
    public ResultMessage<Object> saveSettings(@RequestBody AppointmentOrderSetting setting) {
        appointmentService.saveAppointmentOrderSetting(setting);
        return ResultUtil.success();
    }
}
