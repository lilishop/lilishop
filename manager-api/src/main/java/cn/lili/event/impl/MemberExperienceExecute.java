package cn.lili.event.impl;

import cn.lili.event.GoodsCommentCompleteEvent;
import cn.lili.event.MemberRegisterEvent;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberEvaluation;
import cn.lili.modules.member.entity.enums.ExperienceRuleEnum;
import cn.lili.modules.member.service.MemberExperienceService;
import cn.lili.modules.member.service.MemberShareRegisterService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 客户经验值
 *
 * @author Bulbasaur
 * @since 2021/5/16 11:16 下午
 */
@Service
public class MemberExperienceExecute implements MemberRegisterEvent, GoodsCommentCompleteEvent, OrderStatusChangeEvent {

    @Autowired
    private MemberExperienceService memberExperienceService;
    @Autowired
    private MemberShareRegisterService memberShareRegisterService;
    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;

    /**
     * 客户注册赠送经验值
     *
     * @param member 客户
     */
    @Override
    public void memberRegister(Member member) {
        memberExperienceService.grantExperience(member.getId(), ExperienceRuleEnum.REGISTER, member.getId(), "客户注册，赠送经验值");
        memberShareRegisterService.handleShareRegisterReward(member);
    }

    /**
     * 商品评价赠送经验值
     *
     * @param memberEvaluation 客户评价
     */
    @Override
    public void goodsComment(MemberEvaluation memberEvaluation) {
        if (memberEvaluation == null || memberEvaluation.getContent() == null) {
            return;
        }
        // 仅评论字数大于30字时才赠送经验值
        String content = memberEvaluation.getContent().replaceAll("\\s+", "");
        if (content.length() <= 30) {
            return;
        }
        memberExperienceService.grantExperience(memberEvaluation.getMemberId(), ExperienceRuleEnum.COMMENT, memberEvaluation.getId(), "客户评价，赠送经验值");
    }

    /**
     * 完成订单赠送经验值
     *
     * @param orderMessage 订单消息
     */
    @Override
    public void orderChange(OrderMessage orderMessage) {
        //获取订单信息
        Order order = orderService.getBySn(orderMessage.getOrderSn());
        if (order == null) {
            return;
        }
        if (orderMessage.getNewStatus().equals(OrderStatusEnum.COMPLETED)) {
            memberExperienceService.grantExperienceByAmount(order.getMemberId(), ExperienceRuleEnum.CONSUME, order.getFlowPrice(), order.getSn(), "客户下单，赠送经验值");
        }
        // 订单支付后触发分享购买奖励
        if (orderMessage.getNewStatus().equals(OrderStatusEnum.PAID)) {
            memberShareRegisterService.handleShareBuyReward(order);
        }
    }
}
