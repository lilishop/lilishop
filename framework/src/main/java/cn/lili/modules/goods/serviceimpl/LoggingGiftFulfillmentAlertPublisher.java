package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.service.GiftFulfillmentAlertPublisher;
import cn.lili.modules.order.order.entity.dos.Order;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * 默认实现：结构化 error 日志，预留后续接入告警通道。
 *
 * @author Mike
 * @date 2026-08-02
 */
@Slf4j
@Service
public class LoggingGiftFulfillmentAlertPublisher implements GiftFulfillmentAlertPublisher {

    @Override
    public void onGiftFulfillmentFailure(Order giftOrder, Order parentOrder, String reason) {
        log.error("满赠电子卡券发卡失败 parentSn={} giftSn={} storeId={} memberId={} reason={}",
                parentOrder == null ? null : parentOrder.getSn(),
                giftOrder == null ? null : giftOrder.getSn(),
                giftOrder == null ? null : giftOrder.getStoreId(),
                giftOrder == null ? null : giftOrder.getMemberId(),
                reason);
    }
}
