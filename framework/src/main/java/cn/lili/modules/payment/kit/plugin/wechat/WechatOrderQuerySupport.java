package cn.lili.modules.payment.kit.plugin.wechat;

import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.dto.PaymentQueryResult;
import cn.lili.modules.system.entity.dto.payment.WechatPaymentSetting;
import com.wechat.pay.java.core.Config;
import com.wechat.pay.java.service.payments.model.Transaction;
import com.wechat.pay.java.service.payments.nativepay.NativePayService;
import com.wechat.pay.java.service.payments.nativepay.model.CloseOrderRequest;
import com.wechat.pay.java.service.payments.nativepay.model.QueryOrderByOutTradeNoRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 微信支付主动查单与关单支持组件
 * 负责调用微信 NativePayService 进行未支付订单主动对账查单，以及订单超时/取消时的安全关单。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class WechatOrderQuerySupport {

    @Autowired
    private WechatConfigSupport wechatConfigSupport;

    /**
     * 主动向微信支付渠道查询订单交易状态
     * 捕获 404 及 ORDERNOTEXIST 异常视为渠道无此单；解析支付成功状态与金额（分转元）并统一映射为 PaymentQueryResult。
     *
     * @param outTradeNo 外部商户单号（对应 Order.payOrderNo）
     * @return 标准化查单结果
     */
    public PaymentQueryResult query(String outTradeNo) {
        if (StringUtils.isEmpty(outTradeNo)) {
            return PaymentQueryResult.builder()
                    .exists(false)
                    .isPaid(false)
                    .isClosed(false)
                    .paymentMethod(PaymentMethodEnum.WECHAT.name())
                    .build();
        }
        try {
            WechatPaymentSetting setting = wechatConfigSupport.getPaymentSetting();
            Config config = wechatConfigSupport.getWechatConfig(setting);
            NativePayService service = new NativePayService.Builder().config(config).build();

            QueryOrderByOutTradeNoRequest request = new QueryOrderByOutTradeNoRequest();
            request.setMchid(setting.getMchId());
            request.setOutTradeNo(outTradeNo);

            Transaction transaction = service.queryOrderByOutTradeNo(request);
            if (transaction == null) {
                return PaymentQueryResult.builder()
                        .exists(false)
                        .isPaid(false)
                        .isClosed(false)
                        .paymentMethod(PaymentMethodEnum.WECHAT.name())
                        .build();
            }

            Transaction.TradeStateEnum tradeState = transaction.getTradeState();
            boolean isPaid = Transaction.TradeStateEnum.SUCCESS.equals(tradeState);
            boolean isClosed = Transaction.TradeStateEnum.CLOSED.equals(tradeState)
                    || Transaction.TradeStateEnum.REVOKED.equals(tradeState);
            Double totalAmount = null;
            if (transaction.getAmount() != null && transaction.getAmount().getTotal() != null) {
                totalAmount = CurrencyUtil.reversalFen(transaction.getAmount().getTotal());
            }

            return PaymentQueryResult.builder()
                    .exists(true)
                    .isPaid(isPaid)
                    .isClosed(isClosed)
                    .paymentMethod(PaymentMethodEnum.WECHAT.name())
                    .receivableNo(transaction.getTransactionId())
                    .totalAmount(totalAmount)
                    .rawStatus(tradeState != null ? tradeState.name() : null)
                    .build();
        } catch (com.wechat.pay.java.core.exception.ServiceException e) {
            if (e.getHttpStatusCode() == 404 || "ORDERNOTEXIST".equalsIgnoreCase(e.getErrorCode())) {
                log.info("微信支付查单：渠道订单不存在，outTradeNo={}", outTradeNo);
                return PaymentQueryResult.builder()
                        .exists(false)
                        .isPaid(false)
                        .isClosed(false)
                        .paymentMethod(PaymentMethodEnum.WECHAT.name())
                        .rawStatus(e.getErrorCode())
                        .errorMessage(e.getErrorMessage())
                        .build();
            }
            log.error("微信支付查单服务异常，outTradeNo={}", outTradeNo, e);
            throw new ServiceException("微信支付查单服务异常: " + e.getErrorMessage());
        } catch (ServiceException se) {
            log.warn("微信支付查单配置异常或未支持，outTradeNo={}", outTradeNo, se);
            return PaymentQueryResult.builder()
                    .exists(false)
                    .isPaid(false)
                    .isClosed(false)
                    .paymentMethod(PaymentMethodEnum.WECHAT.name())
                    .errorMessage(se.getMessage())
                    .build();
        } catch (Exception e) {
            log.error("微信支付查单未知异常，outTradeNo={}", outTradeNo, e);
            throw new ServiceException("微信支付查单未知异常: " + e.getMessage());
        }
    }

    /**
     * 关闭微信渠道未支付订单
     * 对已不存在或已关闭的订单做幂等容错处理，不中断业务流程。
     *
     * @param outTradeNo 外部商户单号（对应 Order.payOrderNo）
     */
    public void close(String outTradeNo) {
        if (StringUtils.isEmpty(outTradeNo)) {
            return;
        }
        try {
            WechatPaymentSetting setting = wechatConfigSupport.getPaymentSetting();
            Config config = wechatConfigSupport.getWechatConfig(setting);
            NativePayService service = new NativePayService.Builder().config(config).build();

            CloseOrderRequest closeRequest = new CloseOrderRequest();
            closeRequest.setMchid(setting.getMchId());
            closeRequest.setOutTradeNo(outTradeNo);
            service.closeOrder(closeRequest);
            log.info("微信支付关单成功，outTradeNo={}", outTradeNo);
        } catch (com.wechat.pay.java.core.exception.ServiceException e) {
            if (e.getHttpStatusCode() == 404 || "ORDERNOTEXIST".equalsIgnoreCase(e.getErrorCode())
                    || "ORDER_CLOSED".equalsIgnoreCase(e.getErrorCode())) {
                log.info("微信支付关单忽略：订单不存在或已关闭，outTradeNo={}, code={}", outTradeNo, e.getErrorCode());
                return;
            }
            log.warn("微信支付关单异常，outTradeNo={}", outTradeNo, e);
        } catch (Exception e) {
            log.warn("微信支付关单异常，outTradeNo={}", outTradeNo, e);
        }
    }
}
