package cn.lili.modules.promotion.service;

import java.util.Collection;
import java.util.Map;

/**
 * 提货订单关联查询，使用礼品卡兑换流水作为持久化依据，不依赖订单表附加字段。
 *
 * @author Bulbasaur
 * @since 2026-11-06
 */
public interface PickupCardOrderService {

    /**
     * 判断订单是否由提货卡兑换产生，订单取消后仍保留此关联。
     * @param orderSn 订单编号
     * @return 存在未逻辑删除的 PICKUP_REDEEM 流水时为 true
     */
    default boolean isPickupOrder(String orderSn) {
        return getCredentialId(orderSn) != null;
    }

    /**
     * 查询订单使用的提货卡凭据 ID。
     * @param orderSn 订单编号，空值不查询
     * @return li_gift_card_cash_credential.id，非提货订单返回 null
     */
    String getCredentialId(String orderSn);

    /**
     * 批量查询当前页订单关联，避免列表逐条查询。
     * @param orderSns 当前页订单编号集合
     * @return 订单编号到提货卡凭据 ID 的映射，不含非提货订单
     */
    Map<String, String> getCredentialIds(Collection<String> orderSns);
}
