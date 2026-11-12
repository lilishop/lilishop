package cn.lili.modules.payment.kit.plugin.wechat;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.core.kit.HttpKit;
import cn.lili.modules.payment.service.RefundLogService;
import cn.lili.modules.system.entity.dto.payment.WechatPaymentSetting;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.wechat.pay.java.core.Config;
import com.wechat.pay.java.core.notification.NotificationConfig;
import com.wechat.pay.java.core.notification.NotificationParser;
import com.wechat.pay.java.core.notification.RequestParam;
import com.wechat.pay.java.service.refund.RefundService;
import com.wechat.pay.java.service.refund.model.AmountReq;
import com.wechat.pay.java.service.refund.model.CreateRequest;
import com.wechat.pay.java.service.refund.model.Refund;
import com.wechat.pay.java.service.refund.model.Status;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import jakarta.servlet.http.HttpServletRequest;

/**
 * 微信支付退款支持组件
 * 负责微信支付退款申请、退款金额转换、渠道退款状态判定与退款日志（RefundLog）持久化，以及退款异步回调通知验签和状态推进。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class WechatRefundSupport {

    @Autowired
    private RefundLogService refundLogService;

    @Autowired
    private OrderService orderService;

    @Autowired
    private WechatConfigSupport wechatConfigSupport;

    /**
     * 发起微信退款申请
     * 将退款金额转换为分（单位：分），组装退款单号与异步通知地址，调用微信 V3 退款接口；若渠道返回关闭或异常状态则阻断并记录异常。
     *
     * @param refundLog   退款日志实体
     * @param notifyUrl   退款异步通知 URL
     * @throws ServiceException 退款申请失败或渠道异常时抛出
     */
    public void refund(RefundLog refundLog, String notifyUrl) {
        try {
            AmountReq amount = new AmountReq();
            amount.setRefund(CurrencyUtil.getFenLong(refundLog.getTotalAmount()));
            Long payTotal = refundLog.getPayPrice() != null && refundLog.getPayPrice() > 0
                    ? CurrencyUtil.getFenLong(refundLog.getPayPrice())
                    : CurrencyUtil.getFenLong(orderService.getPaymentTotal(refundLog.getOrderSn()));
            amount.setTotal(payTotal);
            amount.setCurrency("CNY");

            WechatPaymentSetting setting = wechatConfigSupport.getPaymentSetting();
            Config config = wechatConfigSupport.getWechatConfig(setting);
            RefundService refundService = new RefundService.Builder().config(config).build();

            CreateRequest request = new CreateRequest();
            request.setTransactionId(refundLog.getPaymentReceivableNo());
            request.setAmount(amount);
            request.setOutRefundNo(refundLog.getOutOrderNo());
            request.setReason(refundLog.getRefundReason());
            request.setNotifyUrl(notifyUrl);

            Refund refund = refundService.create(request);
            log.info("微信退款响应 outRefundNo={}, result={}", refundLog.getOutOrderNo(), refund);

            if (refund != null) {
                if (Status.CLOSED.equals(refund.getStatus()) || Status.ABNORMAL.equals(refund.getStatus())) {
                    refundLog.setErrorMessage("微信退款状态异常: " + refund.getStatus());
                    refundLogService.save(refundLog);
                    throw new ServiceException(ResultCode.PAY_ERROR, "微信退款失败: " + refund.getStatus());
                }
                refundLog.setReceivableNo(refund.getRefundId());
                if (Status.SUCCESS.equals(refund.getStatus())) {
                    refundLog.setIsRefund(true);
                }
            }
            refundLogService.save(refundLog);
        } catch (com.wechat.pay.java.core.exception.ServiceException e) {
            log.error("微信退款申请接口异常，orderSn={}, errorCode={}, errorMessage={}",
                    refundLog.getOrderSn(), e.getErrorCode(), e.getErrorMessage(), e);
            throw new ServiceException(ResultCode.PAY_ERROR, "微信退款申请失败: " + e.getErrorMessage());
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            log.error("微信退款申请失败，orderSn={}", refundLog.getOrderSn(), e);
            throw new ServiceException(ResultCode.PAY_ERROR, "微信退款申请失败: " + (e.getMessage() != null ? e.getMessage() : "渠道异常"));
        }
    }

    /**
     * 处理微信退款异步通知
     * 校验微信支付签名、解密退款通知报文，并幂等更新退款日志退款状态
     *
     * @param request 包含微信退款通知请求头的 HTTP 请求
     */
    public void refundNotify(HttpServletRequest request) {
        RequestParam requestParam = new RequestParam.Builder()
                .serialNumber(request.getHeader("Wechatpay-Serial"))
                .nonce(request.getHeader("Wechatpay-Nonce"))
                .signature(request.getHeader("Wechatpay-Signature"))
                .timestamp(request.getHeader("Wechatpay-Timestamp"))
                .body(HttpKit.readData(request))
                .build();

        NotificationConfig config = wechatConfigSupport.getNotificationConfig();
        NotificationParser parser = new NotificationParser(config);

        try {
            Refund refund = parser.parse(requestParam, Refund.class);
            RefundLog refundLog = refundLogService.getOne(new LambdaQueryWrapper<RefundLog>()
                    .eq(RefundLog::getPaymentReceivableNo, refund.getTransactionId()));
            if (refundLog != null) {
                if (refund.getStatus() != null && Status.SUCCESS.equals(refund.getStatus())) {
                    refundLog.setIsRefund(true);
                    refundLog.setReceivableNo(refund.getRefundId());
                } else {
                    refundLog.setIsRefund(false);
                    refundLog.setErrorMessage("微信退款异步通知非成功状态: " + refund.getStatus());
                }
                refundLogService.saveOrUpdate(refundLog);
            }
        } catch (Exception e) {
            log.error("微信退款异步通知处理失败", e);
        }
    }
}
