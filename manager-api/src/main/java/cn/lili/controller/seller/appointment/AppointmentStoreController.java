package cn.lili.controller.seller.appointment;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.appointment.entity.dos.AppointmentServiceStaff;
import cn.lili.modules.appointment.entity.dos.AppointmentStoreConfig;
import cn.lili.modules.appointment.entity.dos.AppointmentSystemForm;
import cn.lili.modules.appointment.entity.dto.AppointmentGoodsExtSaveDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentSystemFormDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentReassignDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentServiceStaffDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentStoreConfigDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentVerifyCodeDTO;
import cn.lili.modules.appointment.entity.dto.search.AppointmentWorkOrderSearchParams;
import cn.lili.modules.appointment.entity.vo.AppointmentCalendarDaySummaryVO;
import cn.lili.modules.appointment.entity.vo.AppointmentCalendarEntryVO;
import cn.lili.modules.appointment.entity.vo.AppointmentGoodsExtVO;
import cn.lili.modules.appointment.entity.vo.AppointmentVerificationOrderVO;
import cn.lili.modules.appointment.entity.vo.AppointmentVerificationResultVO;
import cn.lili.modules.appointment.entity.vo.AppointmentWorkOrderVO;
import cn.lili.modules.appointment.service.AppointmentService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 店铺端预约服务接口
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@RestController
@Tag(name = "店铺端,预约服务")
@RequestMapping("/store/appointment")
public class AppointmentStoreController {

    @Autowired
    private AppointmentService appointmentService;

    @Operation(summary = "保存预约商品扩展设置")
    @PostMapping("/goods-ext")
    public ResultMessage<Object> saveGoodsExt(@Valid @RequestBody AppointmentGoodsExtSaveDTO dto) {
        appointmentService.saveGoodsExt(dto);
        return ResultUtil.success();
    }

    @Operation(summary = "查询预约商品扩展设置")
    @GetMapping("/goods-ext/{goodsId}")
    public ResultMessage<AppointmentGoodsExtVO> getGoodsExt(@PathVariable String goodsId) {
        return ResultUtil.data(appointmentService.getGoodsExt(goodsId));
    }

    @Operation(summary = "服务人员分页")
    @GetMapping({"/staff", "/service-staff"})
    public ResultMessage<IPage<AppointmentServiceStaff>> pageStaff(AppointmentWorkOrderSearchParams params) {
        String storeId = UserContext.getCurrentUser().getStoreId();
        return ResultUtil.data(appointmentService.pageStaff(storeId, params));
    }

    @Operation(summary = "启用服务人员列表")
    @GetMapping("/service-staff/enabled")
    public ResultMessage<List<AppointmentServiceStaff>> listEnabledStaff() {
        return ResultUtil.data(appointmentService.listEnabledStaff(UserContext.getCurrentUser().getStoreId()));
    }

    @Operation(summary = "保存服务人员")
    @PostMapping({"/staff", "/service-staff"})
    public ResultMessage<AppointmentServiceStaff> saveStaff(@Valid @RequestBody AppointmentServiceStaffDTO dto) {
        String storeId = UserContext.getCurrentUser().getStoreId();
        return ResultUtil.data(appointmentService.saveStaff(storeId, dto));
    }

    @Operation(summary = "删除服务人员")
    @DeleteMapping({"/staff/{id}", "/service-staff/{id}"})
    public ResultMessage<Object> deleteStaff(@PathVariable String id) {
        appointmentService.deleteStaff(UserContext.getCurrentUser().getStoreId(), id);
        return ResultUtil.success();
    }

    @Operation(summary = "店铺预约配置列表")
    @GetMapping("/config")
    public ResultMessage<List<AppointmentStoreConfig>> listConfig() {
        return ResultUtil.data(appointmentService.listStoreConfig(UserContext.getCurrentUser().getStoreId()));
    }

    @Operation(summary = "保存店铺预约配置")
    @PostMapping("/config")
    public ResultMessage<Object> saveConfig(@Valid @RequestBody AppointmentStoreConfigDTO dto) {
        appointmentService.saveStoreConfig(UserContext.getCurrentUser().getStoreId(), dto);
        return ResultUtil.success();
    }

    @Operation(summary = "到店服务设置")
    @GetMapping("/settings/in-store")
    public ResultMessage<Map<String, Object>> getInStoreSettings() {
        return ResultUtil.data(appointmentService.getInStoreSettings(UserContext.getCurrentUser().getStoreId()));
    }

    @Operation(summary = "保存到店服务设置")
    @PutMapping("/settings/in-store")
    public ResultMessage<Object> saveInStoreSettings(@RequestBody Map<String, Object> form) {
        appointmentService.saveInStoreSettings(UserContext.getCurrentUser().getStoreId(), form);
        return ResultUtil.success();
    }

    @Operation(summary = "上门服务设置")
    @GetMapping("/settings/on-site")
    public ResultMessage<Map<String, Object>> getOnSiteSettings() {
        return ResultUtil.data(appointmentService.getOnSiteSettings(UserContext.getCurrentUser().getStoreId()));
    }

    @Operation(summary = "保存上门服务设置")
    @PutMapping("/settings/on-site")
    public ResultMessage<Object> saveOnSiteSettings(@RequestBody Map<String, Object> form) {
        appointmentService.saveOnSiteSettings(UserContext.getCurrentUser().getStoreId(), form);
        return ResultUtil.success();
    }

    @Operation(summary = "工单分页")
    @GetMapping("/work-orders")
    public ResultMessage<IPage<AppointmentWorkOrderVO>> pageWorkOrders(AppointmentWorkOrderSearchParams params) {
        params.setStoreId(UserContext.getCurrentUser().getStoreId());
        return ResultUtil.data(appointmentService.pageWorkOrders(params, false));
    }

    @Operation(summary = "工单状态统计")
    @GetMapping("/work-orders/status-counts")
    public ResultMessage<Map<String, Long>> workOrderStatusCounts() {
        return ResultUtil.data(appointmentService.countWorkOrderStatus(UserContext.getCurrentUser().getStoreId()));
    }

    @Operation(summary = "领取工单")
    @PostMapping("/work-orders/{id}/claim")
    public ResultMessage<Object> claimWorkOrder(@PathVariable String id) {
        appointmentService.claimWorkOrder(
                UserContext.getCurrentUser().getStoreId(),
                UserContext.getCurrentUser().getId(),
                id);
        return ResultUtil.success();
    }

    @Operation(summary = "改派服务人员")
    @PostMapping("/work-orders/{id}/reassign")
    public ResultMessage<Object> reassignWorkOrder(@PathVariable String id,
                                                   @Valid @RequestBody AppointmentReassignDTO dto) {
        appointmentService.reassignWorkOrder(UserContext.getCurrentUser().getStoreId(), dto, id);
        return ResultUtil.success();
    }

    @Operation(summary = "结束服务")
    @PostMapping("/work-orders/{id}/end-service")
    public ResultMessage<Object> endService(@PathVariable String id) {
        appointmentService.endService(UserContext.getCurrentUser().getStoreId(), id);
        return ResultUtil.success();
    }

    @Operation(summary = "按订单号查询预约核销信息")
    @GetMapping("/verification/order-by-sn/{orderSn}")
    public ResultMessage<AppointmentVerificationOrderVO> getVerificationByOrderSn(@PathVariable String orderSn) {
        return ResultUtil.data(appointmentService.getVerificationByOrderSn(
                UserContext.getCurrentUser().getStoreId(), orderSn));
    }

    @Operation(summary = "按核销码查询预约订单")
    @GetMapping("/verification/order/{code}")
    public ResultMessage<AppointmentVerificationOrderVO> getOrderByVerificationCode(@PathVariable String code) {
        return ResultUtil.data(appointmentService.getOrderByVerificationCode(
                UserContext.getCurrentUser().getStoreId(), code));
    }

    @Operation(summary = "核销预约码")
    @PostMapping("/verification/verify")
    public ResultMessage<AppointmentVerificationResultVO> verifyCode(@Valid @RequestBody AppointmentVerifyCodeDTO dto) {
        return ResultUtil.data(appointmentService.verifyCode(
                UserContext.getCurrentUser().getStoreId(),
                UserContext.getCurrentUser().getId(),
                dto));
    }

    @Operation(summary = "服务月历汇总")
    @GetMapping("/calendar")
    public ResultMessage<Map<String, Object>> calendarMonth(@RequestParam String month) {
        List<AppointmentCalendarDaySummaryVO> days = appointmentService.calendarMonthSummary(
                UserContext.getCurrentUser().getStoreId(), month);
        return ResultUtil.data(Map.of("days", days));
    }

    @Operation(summary = "服务月历当日明细")
    @GetMapping("/calendar/day")
    public ResultMessage<Map<String, Object>> calendarDay(@RequestParam @DateTimeFormat(pattern = "yyyy-MM-dd") Date date,
                                                          @RequestParam(required = false, defaultValue = "ALL") String status) {
        List<AppointmentCalendarEntryVO> records = appointmentService.calendarDayEntries(
                UserContext.getCurrentUser().getStoreId(), date, status);
        return ResultUtil.data(Map.of("records", records));
    }

    @Operation(summary = "系统表单分页")
    @GetMapping("/system-forms")
    public ResultMessage<IPage<AppointmentSystemForm>> pageSystemForms(
            @RequestParam(defaultValue = "1") int pageNumber,
            @RequestParam(defaultValue = "10") int pageSize,
            @RequestParam(required = false) String formName,
            @RequestParam(required = false) String status) {
        return ResultUtil.data(appointmentService.pageSystemForms(
                UserContext.getCurrentUser().getStoreId(), pageNumber, pageSize, formName, status));
    }

    @Operation(summary = "启用系统表单列表")
    @GetMapping("/system-forms/enabled")
    public ResultMessage<List<AppointmentSystemForm>> listEnabledSystemForms() {
        return ResultUtil.data(appointmentService.listEnabledSystemForms(UserContext.getCurrentUser().getStoreId()));
    }

    @Operation(summary = "系统表单详情")
    @GetMapping("/system-forms/{id}")
    public ResultMessage<AppointmentSystemForm> getSystemForm(@PathVariable String id) {
        return ResultUtil.data(appointmentService.getSystemForm(UserContext.getCurrentUser().getStoreId(), id));
    }

    @Operation(summary = "保存系统表单")
    @PostMapping("/system-forms")
    public ResultMessage<AppointmentSystemForm> saveSystemForm(@Valid @RequestBody AppointmentSystemFormDTO dto) {
        return ResultUtil.data(appointmentService.saveSystemForm(UserContext.getCurrentUser().getStoreId(), dto));
    }

    @Operation(summary = "删除系统表单")
    @DeleteMapping("/system-forms/{id}")
    public ResultMessage<Object> deleteSystemForm(@PathVariable String id) {
        appointmentService.deleteSystemForm(UserContext.getCurrentUser().getStoreId(), id);
        return ResultUtil.success();
    }
}
