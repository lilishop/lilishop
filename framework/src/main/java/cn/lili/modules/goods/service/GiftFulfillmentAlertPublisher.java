package cn.lili.modules.goods.service;

import cn.lili.modules.order.order.entity.dos.Order;

/**
 * 满赠 E_COUPON 子单发卡失败告警扩展点（P1 可接站内信/监控）。
 *
 * @author Mike
 * @date 2026-08-02
 */
public interface GiftFulfillmentAlertPublisher {

    /**
     * 记录赠品子单发卡失败，供后续人工补偿或告警。
     */
    void onGiftFulfillmentFailure(Order giftOrder, Order parentOrder, String reason);
}
