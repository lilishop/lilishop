package cn.lili.modules.payment.kit.params.impl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import cn.lili.modules.payment.entity.enums.CashierEnum;
import cn.lili.modules.payment.kit.params.CashierExecute;
import cn.lili.modules.payment.kit.params.dto.CashierParam;
import cn.lili.modules.system.entity.dto.BaseSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 订单支付信息获取
 *
 * @author Chopper
 * @since 2021-01-25 20:00
 */
@Slf4j
@Component
public class OrderCashier implements CashierExecute {
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

    @Override
    public CashierEnum cashierEnum() {
        return CashierEnum.ORDER;
    }

    @Override
    public CashierParam getPaymentParams(PayParam payParam) {
        if (payParam.getOrderType().equals(CashierEnum.ORDER.name())) {
            //准备返回的数据
            CashierParam cashierParam = new CashierParam();
            //订单信息获取
            OrderDetailVO order = orderService.queryDetail(payParam.getSn());

            //如果订单已支付，则不能发器支付
            if (order.getOrder().getPayStatus().equals(PayStatusEnum.PAID.name())) {
                throw new ServiceException(ResultCode.PAY_DOUBLE_ERROR);
            }
            //如果订单状态不是待付款，则抛出异常
            if (!order.getOrder().getOrderStatus().equals(OrderStatusEnum.UNPAID.name())) {
                throw new ServiceException(ResultCode.PAY_BAN);
            }
            cashierParam.setPrice(order.getOrder().getFlowPrice());

            try {
                BaseSetting baseSetting = JSONUtil.toBean(settingService.get(SettingEnum.BASE_SETTING.name()).getSettingValue(), BaseSetting.class);
                cashierParam.setTitle(baseSetting.getSiteName());
            } catch (Exception e) {
                cashierParam.setTitle("多用户商城，在线支付");
            }


            List<OrderItem> orderItemList = order.getOrderItems();
            StringBuilder subject = new StringBuilder();
            for (OrderItem orderItem : orderItemList) {
                subject.append(orderItem.getGoodsName()).append(";");
            }

            cashierParam.setDetail(subject.toString());

            cashierParam.setOrderSns(payParam.getSn());
            cashierParam.setCreateTime(order.getOrder().getCreateTime());
            return cashierParam;
        }

        return null;
    }

    @Override
    public void paymentSuccess(PaymentSuccessParams paymentSuccessParams) {

        PayParam payParam = paymentSuccessParams.getPayParam();
        if (payParam.getOrderType().equals(CashierEnum.ORDER.name())) {
            orderService.payOrder(payParam.getSn(),
                    paymentSuccessParams.getPaymentMethod(),
                    paymentSuccessParams.getReceivableNo());
            log.info("订单{}支付成功,金额{},方式{}", payParam.getSn(),
                    paymentSuccessParams.getPaymentMethod(),
                    paymentSuccessParams.getReceivableNo());
        }
    }

    @Override
    public Boolean paymentResult(PayParam payParam) {
        if (payParam.getOrderType().equals(CashierEnum.ORDER.name())) {
            Order order = orderService.getBySn(payParam.getSn());
            if (order != null) {
                return PayStatusEnum.PAID.name().equals(order.getPayStatus());
            } else {
                throw new ServiceException(ResultCode.PAY_NOT_EXIST_ORDER);
            }
        }
        return false;
    }
}
