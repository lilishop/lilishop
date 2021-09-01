package cn.lili.event.impl;

import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 积分
 *
 * @author Chopper
 * @since 2021-03-13 16:58
 */
@Slf4j
@Service
public class PointExecute implements OrderStatusChangeEvent {

    @Autowired
    private MemberService memberService;

    @Autowired
    private OrderService orderService;

    @Override
    public void orderChange(OrderMessage orderMessage) {

        switch (orderMessage.getNewStatus()) {
            case CANCELLED:
                Order order = orderService.getBySn(orderMessage.getOrderSn());
                Long point = order.getPriceDetailDTO().getPayPoint();
                if (point <= 0) {
                    return;
                }
                //如果未付款，则不去要退回相关代码执行
                if (order.getPayStatus().equals(PayStatusEnum.UNPAID.name())) {
                    return;
                }
                //如果他不处于连续赠送阶段，则只赠送签到积分数
                String content = "订单取消，积分返还：" + point + "分";
                //赠送会员积分
                memberService.updateMemberPoint(point, PointTypeEnum.INCREASE.name(), order.getMemberId(), content);
                break;
            default:
                break;
        }


    }


}
