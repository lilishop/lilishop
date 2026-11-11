package cn.lili.modules.payment.kit.plugin.wallet;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.entity.enums.CashierEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.CashierSupport;
import cn.lili.modules.payment.kit.Payment;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import cn.lili.modules.payment.kit.params.dto.CashierParam;
import cn.lili.modules.payment.service.PaymentService;
import cn.lili.modules.payment.service.RefundLogService;
import cn.lili.modules.wallet.entity.dto.MemberWalletUpdateDTO;
import cn.lili.modules.wallet.entity.enums.DepositServiceTypeEnum;
import cn.lili.modules.wallet.service.MemberWalletService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

/**
 * WalletPlugin
 *
 * @author Chopper
 * @version v1.0
 * 2021-02-20 10:14
 */
@Slf4j
@Component
public class WalletPlugin implements Payment {

    /**
     * 支付日志
     */
    @Autowired
    private PaymentService paymentService;
    /**
     * 退款日志
     */
    @Autowired
    private RefundLogService refundLogService;
    /**
     * 客户余额
     */
    @Autowired
    private MemberWalletService memberWalletService;
    /**
     * 收银台
     */
    @Autowired
    private CashierSupport cashierSupport;

    @Autowired
    private RedissonClient redisson;

    @Override
    public ResultMessage<Object> h5pay(HttpServletRequest request, HttpServletResponse response, PayParam payParam) {
        savePaymentLog(payParam);
        return ResultUtil.success(ResultCode.PAY_SUCCESS);
    }

    @Override
    public ResultMessage<Object> jsApiPay(HttpServletRequest request, PayParam payParam) {
        savePaymentLog(payParam);
        return ResultUtil.success(ResultCode.PAY_SUCCESS);
    }

    @Override
    public ResultMessage<Object> appPay(HttpServletRequest request, PayParam payParam) {
        savePaymentLog(payParam);
        return ResultUtil.success(ResultCode.PAY_SUCCESS);
    }

    @Override
    public ResultMessage<Object> nativePay(HttpServletRequest request, PayParam payParam) {
        if (payParam.getOrderType().equals(CashierEnum.RECHARGE.name())) {
            throw new ServiceException(ResultCode.CAN_NOT_RECHARGE_WALLET);
        }
        savePaymentLog(payParam);
        return ResultUtil.success(ResultCode.PAY_SUCCESS);
    }

    @Override
    public ResultMessage<Object> mpPay(HttpServletRequest request, PayParam payParam) {

        savePaymentLog(payParam);
        return ResultUtil.success(ResultCode.PAY_SUCCESS);
    }

    /**
     * 保存支付日志
     *
     * @param payParam 支付参数
     */
    private void savePaymentLog(PayParam payParam) {
        //同一个客户如果在不同的客户端使用预存款支付，会存在同时支付，无法保证预存款的正确性，所以对客户加锁
        RLock lock = redisson.getLock(UserContext.getCurrentUser().getId() + "");
        lock.lock();
        try {
            //获取支付收银参数
            CashierParam cashierParam = cashierSupport.cashierParam(payParam);
            if (cashierParam.getSupport() == null || !cashierParam.getSupport().contains(PaymentMethodEnum.WALLET.name())) {
                log.warn("余额支付被拒绝：当前客户端[{}]未启用余额支付，sn={}", payParam.getClientType(), payParam.getSn());
                throw new ServiceException(ResultCode.PAY_NOT_SUPPORT);
            }
            this.callBack(payParam, cashierParam);
        } finally {
            lock.unlock();
        }
    }

    /**
     * 余额退款：按会员加锁并校验退款流水，防止订单取消、重复通知等场景同一笔退款重复入账。
     * 幂等口径为 订单编号+售后单号：取消退款售后单号固定为 ORDER_CANCEL，真实售后退款的售后单号本身唯一。
     */
    @Override
    public void refund(RefundLog refundLog) {
        RLock lock = redisson.getLock("WALLET_REFUND_" + refundLog.getMemberId());
        lock.lock();
        try {
            long refunded = refundLogService.count(new LambdaQueryWrapper<RefundLog>()
                    .eq(RefundLog::getOrderSn, refundLog.getOrderSn())
                    .eq(RefundLog::getAfterSaleNo, refundLog.getAfterSaleNo())
                    .eq(RefundLog::getIsRefund, true));
            if (refunded > 0) {
                log.warn("重复退款请求已忽略：订单[{}]，售后单[{}]", refundLog.getOrderSn(), refundLog.getAfterSaleNo());
                return;
            }
            memberWalletService.increase(new MemberWalletUpdateDTO(refundLog.getTotalAmount(),
                    refundLog.getMemberId(),
                    buildRefundDetail(refundLog),
                    DepositServiceTypeEnum.WALLET_REFUND.name()));
            refundLog.setIsRefund(true);
            refundLogService.save(refundLog);
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            log.error("余额退款失败", e);
            throw new ServiceException(ResultCode.PAY_ERROR, "余额退款失败: " + (e.getMessage() != null ? e.getMessage() : "未知异常"));
        } finally {
            lock.unlock();
        }
    }

    /**
     * 组装余额退款的变动日志详情。
     * 订单取消退款的售后单号是占位值 ORDER_CANCEL，对客户展示为“订单取消”；
     * 真实售后退款保留售后单号，便于按售后单核对退款来源。
     *
     * @param refundLog 退款日志
     * @return 变动日志详情
     */
    private String buildRefundDetail(RefundLog refundLog) {
        if ("ORDER_CANCEL".equals(refundLog.getAfterSaleNo())) {
            return "订单[" + refundLog.getOrderSn() + "]，订单取消，退还金额[" + refundLog.getTotalAmount() + "]";
        }
        if (refundLog.getAfterSaleNo() != null && refundLog.getAfterSaleNo().startsWith("DUP_PAY")) {
            return "订单[" + refundLog.getOrderSn() + "]，重复付款原路退回，退还金额[" + refundLog.getTotalAmount() + "]";
        }
        if (refundLog.getAfterSaleNo() != null && refundLog.getAfterSaleNo().startsWith("LATE_PAY")) {
            return "订单[" + refundLog.getOrderSn() + "]，订单取消迟到付款原路退回，退还金额[" + refundLog.getTotalAmount() + "]";
        }
        return "订单[" + refundLog.getOrderSn() + "]，售后单[" + refundLog.getAfterSaleNo() + "]，退还金额[" + refundLog.getTotalAmount() + "]";
    }

    /**
     * 支付订单
     *
     * @param payParam     支付参数
     * @param cashierParam 收银台参数
     */
    public void callBack(PayParam payParam, CashierParam cashierParam) {

        //支付信息
        try {
            if (UserContext.getCurrentUser() == null) {
                throw new ServiceException(ResultCode.USER_NOT_LOGIN);
            }
            boolean result = memberWalletService.reduce(new MemberWalletUpdateDTO(
                            cashierParam.getPrice(),
                            UserContext.getCurrentUser().getId(),
                            "订单[" + cashierParam.getOrderSns() + "]支付金额[" + cashierParam.getPrice() + "]",
                            DepositServiceTypeEnum.WALLET_PAY.name()
                    )
            );
            if (result) {
                try {
                    PaymentSuccessParams paymentSuccessParams = new PaymentSuccessParams(
                            PaymentMethodEnum.WALLET.name(),
                            "",
                            cashierParam.getPrice(),
                            payParam
                    );

                    paymentService.success(paymentSuccessParams);
                    log.info("支付回调通知：余额支付：{}", payParam);
                } catch (ServiceException e) {
                    //业务异常，则支付手动回滚
                    memberWalletService.increase(new MemberWalletUpdateDTO(
                            cashierParam.getPrice(),
                            UserContext.getCurrentUser().getId(),
                            "订单[" + cashierParam.getOrderSns() + "]支付异常，余额返还[" + cashierParam.getPrice() + "]",
                            DepositServiceTypeEnum.WALLET_REFUND.name())
                    );
                    throw e;
                }
            } else {
                throw new ServiceException(ResultCode.WALLET_INSUFFICIENT);
            }
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            log.info("余额支付异常", e);
            throw new ServiceException(ResultCode.WALLET_INSUFFICIENT);
        }

    }

}
