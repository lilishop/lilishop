package cn.lili.event.impl;

import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.wechat.service.WechatMPService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 微信小程序执行器
 *
 * @author Chopper
 * @version v1.0 2021-04-19 14:25
 */
@Slf4j
@Service
public class WechatMPExecute implements OrderStatusChangeEvent {

    @Autowired
    private WechatMPService wechatMPService;


    /**
     * 支付、部分发货及交付节点记录同步进度，整笔支付全部履约后提交发货快照；完成时先补齐发货再通知收货。
     *
     * @param orderMessage 订单消息
     */
    @Override
    public void orderChange(OrderMessage orderMessage) {

        switch (orderMessage.getNewStatus()) {
            case PAID:
            case UNDELIVERED:
            case PARTS_DELIVERED:
            case TAKE:
            case STAY_PICKED_UP:
            case DELIVERED:
                try {
                    wechatMPService.uploadShippingInfo(orderMessage.getOrderSn());
                } catch (Exception e) {
                    log.error("发货信息录入失败", e);
                }
                break;
            case COMPLETED:
                try {
                    wechatMPService.uploadShippingInfo(orderMessage.getOrderSn());
                    wechatMPService.notifyConfirmReceive(orderMessage.getOrderSn());
                } catch (Exception e) {
                    log.error("微信消息发送失败", e);
                }
                break;
            default:
                break;
        }
    }


}
