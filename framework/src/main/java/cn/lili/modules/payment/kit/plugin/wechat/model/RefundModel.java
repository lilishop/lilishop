package cn.lili.modules.payment.kit.plugin.wechat.model;

import lombok.Data;
import lombok.experimental.Accessors;


/**
 * 国内退款-退款申请
 *
 * @author Chopper
 * @since 2020/12/17 17:58
 */
@Data
@Accessors(chain = true)
public class RefundModel {


    /**
     * 原支付交易对应的微信订单号
     */
    private String transaction_id;

    /**
     * 商户订单号
     */
    private String out_trade_no;

    /**
     * 商户退款单号
     */
    private String out_refund_no;

    /**
     * 退款理由
     */
    private String reason;

    /**
     * 退款金额
     */
    private Amount amount;

    /**
     * 通知地址
     */
    private String notify_url;

}


