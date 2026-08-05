package cn.lili.modules.distribution.service;

/**
 * 分销佣金奖励服务（支付冻结、完成结算、退款冲回）。
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public interface DistributionRewardService {

    /**
     * 订单支付成功后写入分销订单并冻结佣金。
     *
     * @param orderSn 订单编号
     */
    void handlePaymentRewards(String orderSn);

    /**
     * 子订单完成时结算对应佣金。
     *
     * @param orderItemSn 子订单编号
     */
    void settleRewardsByOrderItem(String orderItemSn);

    /**
     * 子订单退款时冲回已冻结或已结算的佣金。
     *
     * @param orderItemSn 子订单编号
     */
    void refundRewardsByOrderItem(String orderItemSn);

    /**
     * 整单取消时释放该订单下所有冻结中的佣金。
     *
     * @param orderSn 订单编号
     */
    void cancelRewardsByOrderSn(String orderSn);

    /**
     * 判断订单是否已存在分销订单记录。
     *
     * @param orderSn 订单编号
     * @return 是否存在
     */
    boolean hasRewardsByOrderSn(String orderSn);

    /**
     * 判断订单项是否已存在分销订单记录。
     *
     * @param orderItemSn 订单明细编号
     * @return 是否存在
     */
    boolean hasRewardsByOrderItemSn(String orderItemSn);
}
