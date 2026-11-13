package cn.lili.modules.payment.kit;

import cn.lili.common.utils.SpringContextUtil;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.wallet.entity.dos.MemberWithdrawApply;
import cn.lili.modules.wallet.entity.dto.TransferResultDTO;
import org.springframework.stereotype.Component;

/**
 * 提现打款插件分发，供钱包与收银台共用，避免钱包依赖包含充值入口的完整收银台。
 * 沿用既有按支付渠道取得插件的机制，不初始化收银入口，不增加重试、状态转换或金额处理。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class PaymentTransferSupport {

    /**
     * 按原有渠道映射调用支付插件打款，返回结果及异常均保持原样交由调用方处理。
     * 本方法不开启数据库事务，外部打款不随数据库事务回滚，不自动重试。
     *
     * @param paymentMethodEnum 原业务入口选定的支付渠道
     * @param memberWithdrawApply 已经原状态闸门校验的提现申请
     * @return 支付插件的打款结果
     */
    public TransferResultDTO transfer(PaymentMethodEnum paymentMethodEnum, MemberWithdrawApply memberWithdrawApply) {
        Payment payment = (Payment) SpringContextUtil.getBean(paymentMethodEnum.getPlugin());
        return payment.transfer(memberWithdrawApply);
    }
}
