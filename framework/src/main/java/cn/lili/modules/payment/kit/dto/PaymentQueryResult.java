package cn.lili.modules.payment.kit.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 支付渠道主动查单结果传输对象
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PaymentQueryResult implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 渠道订单是否存在（true：渠道存在该交易；false：渠道无此交易记录）
     */
    private Boolean exists;

    /**
     * 是否已支付成功（true：已支付成功；false：未支付、支付中或失败）
     */
    private Boolean isPaid;

    /**
     * 是否已关闭或已撤销（true：渠道端交易已关闭；false：未关闭）
     */
    private Boolean isClosed;

    /**
     * 支付渠道枚举名称，取值对应 PaymentMethodEnum（如 WECHAT、ALIPAY）
     */
    private String paymentMethod;

    /**
     * 支付渠道交易流水号（微信 transaction_id 或支付宝 trade_no）
     */
    private String receivableNo;

    /**
     * 实际支付金额，单位：元
     */
    private Double totalAmount;

    /**
     * 渠道原始交易状态代码（如微信 SUCCESS/NOTPAY/CLOSED，支付宝 TRADE_SUCCESS/WAIT_BUYER_PAY/TRADE_CLOSED）
     */
    private String rawStatus;

    /**
     * 错误信息或异常描述
     */
    private String errorMessage;
}
