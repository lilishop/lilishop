package cn.lili.modules.payment.kit.plugin.wechat.model;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 统一下单-结算信息
 *
 * @author Chopper
 * @since 2020/12/17 17:58
 */
@Data
@Accessors(chain = true)
public class SettleInfo {
    /**
     * 是否指定分账
     */
    private boolean profit_sharing;
    /**
     * 补差金额
     */
    private int subsidy_amount;
}
