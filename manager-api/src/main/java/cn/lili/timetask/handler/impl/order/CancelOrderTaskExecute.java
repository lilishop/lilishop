package cn.lili.timetask.handler.impl.order;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.payment.entity.enums.CashierEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.CashierSupport;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.kit.dto.PaymentQueryResult;
import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import cn.lili.modules.payment.service.PaymentService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.OrderSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.timetask.handler.EveryMinuteExecute;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 订单自动取消（每分钟执行）
 *
 * @author paulG
 * @since 2021/3/11
 **/
@Slf4j
@Component
public class CancelOrderTaskExecute implements EveryMinuteExecute {
    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;
    /**
     * 设置
     */
    @Autowired
    private SettingService settingService;
    /**
     * 收银台工具支持
     */
    @Autowired
    private CashierSupport cashierSupport;
    /**
     * 支付业务
     */
    @Autowired
    private PaymentService paymentService;

    @Override
    public void execute() {
        Setting setting = settingService.get(SettingEnum.ORDER_SETTING.name());
        OrderSetting orderSetting = JSONUtil.toBean(setting.getSettingValue(), OrderSetting.class);
        if (orderSetting != null && orderSetting.getAutoCancel() != null) {
            //订单自动取消时间 = 当前时间 - 自动取消时间分钟数
            DateTime cancelTime = DateUtil.offsetMinute(DateUtil.date(), -orderSetting.getAutoCancel());
            LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.eq(Order::getOrderStatus, OrderStatusEnum.UNPAID.name());
            //订单创建时间 <= 订单自动取消时间
            queryWrapper.le(Order::getCreateTime, cancelTime);
            List<Order> list = orderService.list(queryWrapper);
            for (Order order : list) {
                try {
                    handleUnpaidOrderCancel(order);
                } catch (Exception e) {
                    log.error("超时订单取消处理失败：orderSn={}", order.getSn(), e);
                }
            }
        }
    }

    /**
     * 处理超时未支付订单的查单补偿与关单取消
     *
     * @param order 超时未支付订单
     */
    public void handleUnpaidOrderCancel(Order order) {
        // 二次查库确认状态，避免并发状态已改变
        Order freshOrder = orderService.getBySn(order.getSn());
        if (freshOrder == null || !OrderStatusEnum.UNPAID.name().equals(freshOrder.getOrderStatus())) {
            log.info("超时关单前复查：订单已非未支付状态，跳过处理，orderSn={}", order.getSn());
            return;
        }

        String outTradeNo = StringUtils.isNotEmpty(freshOrder.getPayOrderNo()) ? freshOrder.getPayOrderNo() : freshOrder.getSn();
        String paymentMethod = freshOrder.getPaymentMethod();

        // 如果既没有发起过第三方支付流水号，也没有选择过支付方式，直接执行系统取消
        if (StringUtils.isEmpty(freshOrder.getPayOrderNo()) && StringUtils.isEmpty(paymentMethod)) {
            orderService.systemCancel(freshOrder.getSn(), "超时未支付自动取消", false);
            log.info("未发起支付的超时订单已取消：orderSn={}", freshOrder.getSn());
            return;
        }

        // 调用微信/支付宝主动查单
        PaymentQueryResult queryResult = queryPaymentChannel(paymentMethod, outTradeNo);

        // 若渠道已支付，则触发补单
        if (queryResult != null && Boolean.TRUE.equals(queryResult.getIsPaid())) {
            log.warn("超时未支付订单在支付渠道已支付成功，触发补单：orderSn={}, paymentMethod={}, receivableNo={}",
                    freshOrder.getSn(), queryResult.getPaymentMethod(), queryResult.getReceivableNo());
            compensatePaidOrder(freshOrder, queryResult);
            return;
        }

        // 未支付再关闭渠道订单
        if (queryResult != null && Boolean.TRUE.equals(queryResult.getExists()) && !Boolean.TRUE.equals(queryResult.getIsClosed())) {
            PaymentMethodEnum methodEnum = PaymentMethodEnum.paymentNameOf(queryResult.getPaymentMethod());
            if (methodEnum != null) {
                try {
                    cashierSupport.close(methodEnum, outTradeNo);
                    log.info("超时未支付订单渠道关单成功：orderSn={}, outTradeNo={}", freshOrder.getSn(), outTradeNo);
                } catch (Exception e) {
                    log.warn("超时未支付订单渠道关单失败：orderSn={}, outTradeNo={}", freshOrder.getSn(), outTradeNo, e);
                }
            }
        }

        // 取消系统订单
        orderService.systemCancel(freshOrder.getSn(), "超时未支付自动取消", false);
        log.info("超时未支付订单系统取消完成：orderSn={}", freshOrder.getSn());
    }

    /**
     * 调用支付渠道查询支付状态
     *
     * @param paymentMethod 支付方式字符串
     * @param outTradeNo    商户订单号
     * @return 查单结果
     */
    private PaymentQueryResult queryPaymentChannel(String paymentMethod, String outTradeNo) {
        PaymentMethodEnum methodEnum = PaymentMethodEnum.paymentNameOf(paymentMethod);
        if (methodEnum == PaymentMethodEnum.WECHAT || methodEnum == PaymentMethodEnum.ALIPAY) {
            return cashierSupport.query(methodEnum, outTradeNo);
        }

        // 若未明确指定渠道，但有外部单号，先查微信，不存在则查支付宝
        try {
            PaymentQueryResult wechatResult = cashierSupport.query(PaymentMethodEnum.WECHAT, outTradeNo);
            if (Boolean.TRUE.equals(wechatResult.getExists())) {
                return wechatResult;
            }
        } catch (Exception e) {
            log.warn("超时关单微信查单异常，尝试支付宝：outTradeNo={}", outTradeNo, e);
        }

        try {
            PaymentQueryResult alipayResult = cashierSupport.query(PaymentMethodEnum.ALIPAY, outTradeNo);
            if (Boolean.TRUE.equals(alipayResult.getExists())) {
                return alipayResult;
            }
        } catch (Exception e) {
            log.warn("超时关单支付宝查单异常：outTradeNo={}", outTradeNo, e);
        }

        return PaymentQueryResult.builder().exists(false).isPaid(false).isClosed(false).build();
    }

    /**
     * 补单处理
     *
     * @param order       订单
     * @param queryResult 查单结果
     */
    private void compensatePaidOrder(Order order, PaymentQueryResult queryResult) {
        PayParam payParam = new PayParam();
        if (StringUtils.isNotEmpty(order.getTradeSn()) && !order.getTradeSn().equals(order.getSn())) {
            payParam.setOrderType(CashierEnum.TRADE.name());
            payParam.setSn(order.getTradeSn());
        } else {
            payParam.setOrderType(CashierEnum.ORDER.name());
            payParam.setSn(order.getSn());
        }

        PaymentSuccessParams successParams = new PaymentSuccessParams(
                queryResult.getPaymentMethod(),
                queryResult.getReceivableNo(),
                queryResult.getTotalAmount(),
                payParam
        );
        paymentService.success(successParams);
    }
}
