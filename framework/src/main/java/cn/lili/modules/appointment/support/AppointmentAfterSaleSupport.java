package cn.lili.modules.appointment.support;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.utils.SpringContextUtil;
import cn.lili.modules.appointment.entity.dos.AppointmentOrderExt;
import cn.lili.modules.appointment.entity.dos.AppointmentVerificationCode;
import cn.lili.modules.appointment.entity.dos.AppointmentWorkOrder;
import cn.lili.modules.appointment.entity.dto.AppointmentNoShowRefundDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentPlatformRefundDTO;
import cn.lili.modules.appointment.entity.enums.AppointmentPlatformRefundTypeEnum;
import cn.lili.modules.appointment.entity.enums.AppointmentServiceModeEnum;
import cn.lili.modules.appointment.entity.enums.AppointmentVerificationCodeStatusEnum;
import cn.lili.modules.appointment.entity.enums.AppointmentWorkOrderStatusEnum;
import cn.lili.modules.appointment.mapper.AppointmentOrderExtMapper;
import cn.lili.modules.appointment.mapper.AppointmentVerificationCodeMapper;
import cn.lili.modules.appointment.mapper.AppointmentWorkOrderMapper;
import cn.lili.modules.message.entity.enums.NoticeMessageNodeEnum;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.enums.RefundStatusEnum;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.support.OrderLifecycleSupport;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.Payment;
import cn.lili.modules.system.entity.dto.AppointmentOrderSetting;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;

/**
 * 预约售后退款、爽约与定时调度业务支持组件
 * <p>
 * 负责退款校验、通道退款发起、全额退款后释放时段与作废核销码，以及爽约自动完结与服务前定时提醒。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Component
public class AppointmentAfterSaleSupport {

    @Autowired
    private AppointmentWorkOrderMapper workOrderMapper;

    @Autowired
    private AppointmentVerificationCodeMapper verificationCodeMapper;

    @Autowired
    private AppointmentOrderExtMapper orderExtMapper;

    @Autowired
    private OrderMapper orderMapper;

    @Autowired
    private OrderLifecycleSupport orderLifecycleSupport;

    @Autowired
    private OrderItemService orderItemService;

    @Autowired
    private AppointmentNoticeSupport appointmentNoticeSupport;

    @Autowired
    private AppointmentSlotStockSupport appointmentSlotStockSupport;

    @Autowired
    private AppointmentVerificationSupport appointmentVerificationSupport;

    @Autowired
    private AppointmentConfigSupport appointmentConfigSupport;

    /**
     * 判断是否预约类型订单
     *
     * @param order 主订单
     * @return 是否预约订单
     */
    public boolean isAppointmentOrder(Order order) {
        return order != null && OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType());
    }

    /**
     * 判断预约订单服务是否已完结（工单服务完结/爽约，或所有核销码均已核销）
     *
     * @param orderSn 订单编号
     * @return 是否已完结
     */
    public boolean isServiceComplete(String orderSn) {
        AppointmentWorkOrder workOrder = workOrderMapper.selectOne(new LambdaQueryWrapper<AppointmentWorkOrder>()
                .eq(AppointmentWorkOrder::getOrderSn, orderSn)
                .eq(AppointmentWorkOrder::getDeleteFlag, false));
        if (workOrder != null && CharSequenceUtil.equalsAny(workOrder.getServiceStatus(),
                AppointmentWorkOrderStatusEnum.SERVICE_ENDED.name(),
                AppointmentWorkOrderStatusEnum.NO_SHOW.name())) {
            return true;
        }
        long total = verificationCodeMapper.selectCount(new LambdaQueryWrapper<AppointmentVerificationCode>()
                .eq(AppointmentVerificationCode::getOrderSn, orderSn)
                .eq(AppointmentVerificationCode::getDeleteFlag, false));
        if (total == 0) {
            return false;
        }
        long unused = verificationCodeMapper.selectCount(new LambdaQueryWrapper<AppointmentVerificationCode>()
                .eq(AppointmentVerificationCode::getOrderSn, orderSn)
                .eq(AppointmentVerificationCode::getStatus, AppointmentVerificationCodeStatusEnum.UNUSED.name())
                .eq(AppointmentVerificationCode::getDeleteFlag, false));
        return unused == 0;
    }

    /**
     * 校验买家是否可发起售后退款
     *
     * @param orderSn 订单编号
     * @return 是否允许发起
     */
    public boolean canApplyBuyerAfterSale(String orderSn) {
        Order order = getOrderBySn(orderSn);
        if (!isAppointmentOrder(order) || !PayStatusEnum.PAID.name().equals(order.getPayStatus())) {
            return false;
        }
        if (OrderStatusEnum.CANCELLED.name().equals(order.getOrderStatus())) {
            return false;
        }
        return !isServiceComplete(orderSn) && getRemainingRefundableAmount(orderSn) > 0;
    }

    /**
     * 计算预约订单剩余可退金额
     *
     * @param orderSn 订单编号
     * @return 剩余可退金额
     */
    public double getRemainingRefundableAmount(String orderSn) {
        Order order = getOrderBySn(orderSn);
        if (order == null) {
            return 0D;
        }
        AppointmentOrderExt ext = getOrderExt(orderSn);
        double refunded = ext == null || ext.getRefundedAmount() == null ? 0D : ext.getRefundedAmount();
        return Math.max(0D, CurrencyUtil.sub(order.getFlowPrice(), refunded));
    }

    /**
     * 校验买家申请退款金额有效性
     *
     * @param orderSn     订单编号
     * @param applyAmount 申请退款金额
     */
    public void validateBuyerAfterSaleAmount(String orderSn, double applyAmount) {
        if (applyAmount <= 0) {
            throw new ServiceException(ResultCode.APPOINTMENT_REFUND_AMOUNT_INVALID);
        }
        if (!canApplyBuyerAfterSale(orderSn)) {
            throw new ServiceException(ResultCode.APPOINTMENT_AFTER_SALE_SERVICE_COMPLETE);
        }
        if (applyAmount > getRemainingRefundableAmount(orderSn)) {
            throw new ServiceException(ResultCode.APPOINTMENT_REFUND_AMOUNT_INVALID);
        }
    }

    /**
     * 退款成功回调处理（累加已退金额，全额退款时作废核销码与归还时段库存）
     *
     * @param orderSn      订单编号
     * @param refundAmount 本次退款金额
     */
    @Transactional(rollbackFor = Exception.class)
    public void onRefundSuccess(String orderSn, double refundAmount) {
        if (refundAmount <= 0) {
            return;
        }
        Order order = orderMapper.selectForUpdateBySn(orderSn);
        if (!isAppointmentOrder(order)) {
            return;
        }
        AppointmentOrderExt ext = getOrderExt(orderSn);
        if (ext == null) {
            return;
        }
        double refunded = ext.getRefundedAmount() == null ? 0D : ext.getRefundedAmount();
        double newRefunded = CurrencyUtil.add(refunded, refundAmount);
        ext.setRefundedAmount(newRefunded);
        orderExtMapper.updateById(ext);
        if (newRefunded < order.getFlowPrice() - 0.001D) {
            return;
        }
        closeAppointmentAfterFullRefund(order, ext);
    }

    /**
     * 平台端强制发起退款
     *
     * @param orderSn 订单编号
     * @param dto     退款参数
     */
    @Transactional(rollbackFor = Exception.class)
    public void platformRefund(String orderSn, AppointmentPlatformRefundDTO dto) {
        Order order = requirePaidAppointmentOrder(orderSn);
        boolean served = isServiceComplete(orderSn);
        if (AppointmentPlatformRefundTypeEnum.SERVED.name().equals(dto.getRefundType()) && !served) {
            throw new ServiceException(ResultCode.APPOINTMENT_PLATFORM_REFUND_TYPE_MISMATCH);
        }
        if (AppointmentPlatformRefundTypeEnum.UNSERVED.name().equals(dto.getRefundType()) && served) {
            throw new ServiceException(ResultCode.APPOINTMENT_PLATFORM_REFUND_TYPE_MISMATCH);
        }
        validateRefundAmount(orderSn, dto.getRefundAmount());
        refundChannel(order, dto.getRefundAmount(), dto.getReason());
        onRefundSuccess(orderSn, dto.getRefundAmount());
        appointmentNoticeSupport.notifyServiceRefunded(order.getMemberId(), getGoodsName(orderSn), orderSn);
    }

    /**
     * 爽约场景退款
     *
     * @param orderSn 订单编号
     * @param dto     退款参数
     */
    @Transactional(rollbackFor = Exception.class)
    public void noShowRefund(String orderSn, AppointmentNoShowRefundDTO dto) {
        Order order = requirePaidAppointmentOrder(orderSn);
        AppointmentOrderExt ext = requireOrderExt(orderSn);
        if (!AppointmentServiceModeEnum.ON_SITE.name().equals(ext.getServiceMode())) {
            throw new ServiceException(ResultCode.APPOINTMENT_NO_SHOW_ON_SITE_ONLY);
        }
        if (isServiceComplete(orderSn)) {
            throw new ServiceException(ResultCode.APPOINTMENT_NO_SHOW_ALREADY_VERIFIED);
        }
        validateRefundAmount(orderSn, dto.getRefundAmount());
        refundChannel(order, dto.getRefundAmount(), dto.getReason());
        onRefundSuccess(orderSn, dto.getRefundAmount());
        appointmentNoticeSupport.notifyServiceRefunded(order.getMemberId(), getGoodsName(orderSn), orderSn);
    }

    /**
     * 定时任务：处理到店预约爽约自动完结
     */
    public void processNoShowAutoComplete() {
        AppointmentOrderSetting cfg = appointmentConfigSupport.getAppointmentOrderSetting();
        if (cfg.getInStoreNoShowAutoCompleteHours() == null || cfg.getInStoreNoShowAutoCompleteHours() <= 0) {
            return;
        }
        List<AppointmentWorkOrder> candidates = workOrderMapper.selectList(new LambdaQueryWrapper<AppointmentWorkOrder>()
                .in(AppointmentWorkOrder::getServiceStatus,
                        AppointmentWorkOrderStatusEnum.WAIT_VERIFY.name(),
                        AppointmentWorkOrderStatusEnum.PENDING_CLAIM.name(),
                        AppointmentWorkOrderStatusEnum.CLAIMED.name())
                .eq(AppointmentWorkOrder::getDeleteFlag, false));
        Date now = new Date();
        for (AppointmentWorkOrder workOrder : candidates) {
            AppointmentOrderExt ext = getOrderExt(workOrder.getOrderSn());
            if (ext == null || isServiceComplete(workOrder.getOrderSn())) {
                continue;
            }
            Date slotEnd = DateUtil.parse(DateUtil.formatDate(ext.getSlotDate()) + " " + ext.getSlotEndTime() + ":00");
            Date deadline = DateUtil.offsetHour(slotEnd, cfg.getInStoreNoShowAutoCompleteHours());
            if (now.before(deadline)) {
                continue;
            }
            completeNoShow(workOrder);
        }
    }

    /**
     * 定时任务：服务开始前通知买家提醒
     */
    public void processServiceStartRemind() {
        AppointmentOrderSetting cfg = appointmentConfigSupport.getAppointmentOrderSetting();
        if (cfg.getServiceStartRemindMemberHours() == null || cfg.getServiceStartRemindMemberHours() <= 0) {
            return;
        }
        List<AppointmentOrderExt> exts = orderExtMapper.selectList(new LambdaQueryWrapper<AppointmentOrderExt>()
                .eq(AppointmentOrderExt::getDeleteFlag, false));
        Date now = new Date();
        for (AppointmentOrderExt ext : exts) {
            if (isServiceComplete(ext.getOrderSn())) {
                continue;
            }
            Order order = getOrderBySn(ext.getOrderSn());
            if (order == null || !PayStatusEnum.PAID.name().equals(order.getPayStatus())
                    || OrderStatusEnum.CANCELLED.name().equals(order.getOrderStatus())) {
                continue;
            }
            Date start = DateUtil.parse(DateUtil.formatDate(ext.getSlotDate()) + " " + ext.getSlotStartTime() + ":00");
            if (!start.after(now)) {
                continue;
            }
            long hours = DateUtil.between(now, start, cn.hutool.core.date.DateUnit.HOUR);
            if (hours > cfg.getServiceStartRemindMemberHours() || hours <= 0) {
                continue;
            }
            appointmentNoticeSupport.notifyMember(order.getMemberId(),
                    NoticeMessageNodeEnum.APPOINTMENT_SERVICE_START_REMIND_MEMBER,
                    getGoodsName(ext.getOrderSn()), ext.getOrderSn());
        }
    }

    /**
     * 发送预约成功系统通知
     *
     * @param orderSn 订单编号
     */
    public void notifyBookingSuccess(String orderSn) {
        Order order = getOrderBySn(orderSn);
        if (order == null) {
            return;
        }
        appointmentNoticeSupport.notifyMember(order.getMemberId(),
                NoticeMessageNodeEnum.APPOINTMENT_BOOKING_SUCCESS_MEMBER,
                getGoodsName(orderSn), orderSn);
    }

    /**
     * 全额退款完结：释放时段库存、作废核销码、工单置为已退款并取消主订单
     */
    public void closeAppointmentAfterFullRefund(Order order, AppointmentOrderExt ext) {
        int quantity = ext.getQuantity() == null ? 1 : ext.getQuantity();
        appointmentSlotStockSupport.restoreSlotStock(ext.getSlotStockId(), quantity);
        appointmentVerificationSupport.voidVerificationCodes(order.getSn());
        workOrderMapper.update(null, new LambdaUpdateWrapper<AppointmentWorkOrder>()
                .eq(AppointmentWorkOrder::getOrderSn, order.getSn())
                .set(AppointmentWorkOrder::getServiceStatus, AppointmentWorkOrderStatusEnum.REFUNDED.name()));
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        order.setCancelReason("预约订单退款完结");
        orderMapper.updateById(order);
        orderItemService.update(new LambdaUpdateWrapper<OrderItem>()
                .eq(OrderItem::getOrderSn, order.getSn())
                .set(OrderItem::getIsRefund, RefundStatusEnum.ALL_REFUND.name()));
    }

    /**
     * 标记爽约并系统自动完成订单
     */
    public void completeNoShow(AppointmentWorkOrder workOrder) {
        workOrder.setServiceStatus(AppointmentWorkOrderStatusEnum.NO_SHOW.name());
        workOrderMapper.updateById(workOrder);
        appointmentVerificationSupport.voidVerificationCodes(workOrder.getOrderSn());
        Order order = getOrderBySn(workOrder.getOrderSn());
        if (order != null && !OrderStatusEnum.COMPLETED.name().equals(order.getOrderStatus())) {
            orderLifecycleSupport.systemComplete(workOrder.getOrderSn());
        }
    }

    /**
     * 确保为已付款预约订单
     */
    public Order requirePaidAppointmentOrder(String orderSn) {
        Order order = getOrderBySn(orderSn);
        if (!isAppointmentOrder(order)) {
            throw new ServiceException(ResultCode.APPOINTMENT_ORDER_NOT_FOUND);
        }
        if (!PayStatusEnum.PAID.name().equals(order.getPayStatus())) {
            throw new ServiceException(ResultCode.APPOINTMENT_AFTER_SALE_UNPAID);
        }
        return order;
    }

    /**
     * 确保获取到预约扩展实体
     */
    public AppointmentOrderExt requireOrderExt(String orderSn) {
        AppointmentOrderExt ext = getOrderExt(orderSn);
        if (ext == null) {
            throw new ServiceException(ResultCode.APPOINTMENT_ORDER_NOT_FOUND);
        }
        return ext;
    }

    /**
     * 获取订单预约扩展实体
     */
    public AppointmentOrderExt getOrderExt(String orderSn) {
        return orderExtMapper.selectOne(new LambdaQueryWrapper<AppointmentOrderExt>()
                .eq(AppointmentOrderExt::getOrderSn, orderSn)
                .eq(AppointmentOrderExt::getDeleteFlag, false));
    }

    /**
     * 校验退款金额是否超出可退上限
     */
    public void validateRefundAmount(String orderSn, double amount) {
        if (amount <= 0 || amount > getRemainingRefundableAmount(orderSn)) {
            throw new ServiceException(ResultCode.APPOINTMENT_REFUND_AMOUNT_INVALID);
        }
    }

    /**
     * 调起原支付渠道退款
     */
    public void refundChannel(Order order, double amount, String reason) {
        RefundLog refundLog = RefundLog.builder()
                .isRefund(false)
                .totalAmount(amount)
                .payPrice(amount)
                .memberId(order.getMemberId())
                .paymentName(order.getPaymentMethod())
                .paymentReceivableNo(order.getReceivableNo())
                .outOrderNo("AP" + SnowFlake.getIdStr())
                .orderSn(order.getSn())
                .refundReason(reason)
                .build();
        PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.paymentNameOf(order.getPaymentMethod());
        Payment payment = (Payment) SpringContextUtil.getBean(paymentMethodEnum.getPlugin());
        payment.refund(refundLog);
    }

    /**
     * 获取订单首个商品名称用于消息推送
     */
    public String getGoodsName(String orderSn) {
        List<OrderItem> items = orderItemService.getByOrderSn(orderSn);
        return CollUtil.isEmpty(items) ? "" : items.get(0).getGoodsName();
    }

    /**
     * 按订单号读取预约业务所需的订单数据，避免为简单读取引入完整订单服务。
     *
     * @param orderSn 订单编号
     * @return 订单，不存在时为null
     */
    private Order getOrderBySn(String orderSn) {
        return orderMapper.selectOne(new LambdaQueryWrapper<Order>().eq(Order::getSn, orderSn));
    }
}
