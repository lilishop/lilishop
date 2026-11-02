package cn.lili.event.impl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.utils.DateUtil;
import cn.lili.event.AfterSaleStatusChangeEvent;
import cn.lili.event.MemberPointChangeEvent;
import cn.lili.event.MemberWithdrawalEvent;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.event.TradeEvent;
import cn.lili.message.MemberMessageOrchestrator;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.MemberPointMessage;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.message.entity.dto.NoticeMessageDTO;
import cn.lili.modules.message.entity.enums.NoticeMessageNodeEnum;
import cn.lili.modules.message.entity.enums.NoticeMessageParameterEnum;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum;
import cn.lili.modules.wallet.entity.dto.MemberWithdrawalMessage;
import cn.lili.modules.wallet.entity.enums.WithdrawStatusEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * 会员消息编排执行器：把业务事件翻译为场景消息 DTO 并交给编排器统一分发。
 * 本执行器运行在队列消费线程中，事件源已是异步，直接一跳调用编排器。
 *
 * @author Bulbasaur
 * @since 2026-11-02
 */
@Slf4j
@Service
public class MemberMessageOrchestratorExecute implements TradeEvent, OrderStatusChangeEvent,
        AfterSaleStatusChangeEvent, MemberPointChangeEvent, MemberWithdrawalEvent {

    @Autowired
    private MemberMessageOrchestrator memberMessageOrchestrator;
    @Autowired
    private OrderService orderService;
    @Autowired
    private MemberService memberService;

    @Override
    public void orderCreate(TradeDTO tradeDTO) {
        // 订单创建通知已废弃，不再发送
    }

    @Override
    public void orderChange(OrderMessage orderMessage) {
        try {
            OrderDetailVO orderDetailVO = orderService.queryDetail(orderMessage.getOrderSn());
            if (orderDetailVO == null || orderDetailVO.getOrder() == null
                    || orderDetailVO.getOrderItems() == null || orderDetailVO.getOrderItems().isEmpty()) {
                return;
            }
            Order order = orderDetailVO.getOrder();
            NoticeMessageNodeEnum nodeEnum = null;
            switch (orderMessage.getNewStatus()) {
                case DELIVERED:
                    nodeEnum = NoticeMessageNodeEnum.ORDER_DELIVER;
                    break;
                case COMPLETED:
                    nodeEnum = NoticeMessageNodeEnum.ORDER_COMPLETE;
                    break;
                case UNDELIVERED:
                    if (OrderPromotionTypeEnum.PINTUAN.name().equals(order.getOrderPromotionType())) {
                        nodeEnum = NoticeMessageNodeEnum.PINTUAN_SUCCESS;
                    }
                    break;
                default:
                    break;
            }
            if (nodeEnum == null) {
                return;
            }
            orchestrate(buildOrderMessage(nodeEnum, order, orderDetailVO));
        } catch (Exception e) {
            log.error("订单状态变更消息编排异常，orderSn={}", orderMessage.getOrderSn(), e);
        }
    }

    private NoticeMessageDTO buildOrderMessage(NoticeMessageNodeEnum nodeEnum, Order order, OrderDetailVO orderDetailVO) {
        Map<String, String> params = new HashMap<>(16);
        params.put("order_sn", order.getSn());
        params.put(NoticeMessageParameterEnum.GOODS.getType(), orderDetailVO.getOrderItems().get(0).getGoodsName());
        StringBuilder goodsInfo = new StringBuilder();
        for (OrderItem item : orderDetailVO.getOrderItems()) {
            goodsInfo.append(item.getGoodsName()).append("*").append(item.getNum()).append("  ");
        }
        params.put("goods_info", goodsInfo.toString().trim());
        params.put("store_name", order.getStoreName());
        params.put("member_name", order.getMemberName());
        params.put("price", order.getFlowPrice() != null ? order.getFlowPrice().toString() : "0");
        if (order.getPaymentTime() != null) {
            params.put("payment_time", DateUtil.toString(order.getPaymentTime(), DateUtil.STANDARD_FORMAT));
        }
        if (nodeEnum == NoticeMessageNodeEnum.ORDER_DELIVER) {
            params.put("logistics_name", CharSequenceUtil.blankToDefault(order.getLogisticsName(), "无需物流"));
            params.put("logistics_no", CharSequenceUtil.blankToDefault(order.getLogisticsNo(), "-"));
            params.put("logistics_time", order.getLogisticsTime() != null
                    ? DateUtil.toString(order.getLogisticsTime(), DateUtil.STANDARD_FORMAT)
                    : DateUtil.toString(new Date(), DateUtil.STANDARD_FORMAT));
        }
        if (nodeEnum == NoticeMessageNodeEnum.ORDER_COMPLETE && order.getCompleteTime() != null) {
            params.put("complete_time", DateUtil.toString(order.getCompleteTime(), DateUtil.STANDARD_FORMAT));
        }
        NoticeMessageDTO dto = new NoticeMessageDTO();
        dto.setMemberId(order.getMemberId());
        dto.setNoticeMessageNodeEnum(nodeEnum);
        dto.setParameter(params);
        dto.setPage("pages/order/orderDetail?sn=" + order.getSn());
        return dto;
    }

    @Override
    public void afterSaleStatusChange(AfterSale afterSale) {
        try {
            if (afterSale == null || afterSale.getServiceStatus() == null) {
                return;
            }
            String status = afterSale.getServiceStatus();
            String auditResult = null;
            if (AfterSaleStatusEnum.PASS.name().equals(status)
                    || AfterSaleStatusEnum.COMPLETE.name().equals(status)
                    || AfterSaleStatusEnum.WAIT_REFUND.name().equals(status)) {
                auditResult = "审核通过";
            } else if (AfterSaleStatusEnum.REFUSE.name().equals(status)
                    || AfterSaleStatusEnum.SELLER_TERMINATION.name().equals(status)) {
                auditResult = "审核未通过";
            }
            if (auditResult == null) {
                return;
            }
            Map<String, String> params = new HashMap<>(8);
            params.put(NoticeMessageParameterEnum.GOODS.getType(), afterSale.getGoodsName());
            params.put("goods_info", afterSale.getGoodsName());
            params.put("refuse", afterSale.getAuditRemark());
            params.put("audit_result", auditResult);
            params.put("after_sale_sn", afterSale.getSn());
            params.put("order_sn", afterSale.getOrderSn());
            NoticeMessageDTO dto = new NoticeMessageDTO();
            dto.setMemberId(afterSale.getMemberId());
            dto.setNoticeMessageNodeEnum(NoticeMessageNodeEnum.AFTER_SALE_AUDIT);
            dto.setParameter(params);
            dto.setPage("pages/order/afterSales/applyDetail?sn=" + afterSale.getSn());
            orchestrate(dto);
        } catch (Exception e) {
            log.error("售后状态变更消息编排异常", e);
        }
    }

    @Override
    public void memberPointChange(MemberPointMessage memberPointMessage) {
        try {
            if (memberPointMessage == null) {
                return;
            }
            Map<String, String> params = new HashMap<>(8);
            long changePoint = memberPointMessage.getPoint() != null ? memberPointMessage.getPoint() : 0L;
            if (PointTypeEnum.INCREASE.name().equals(memberPointMessage.getType())) {
                params.put("expenditure_points", "0");
                params.put("income_points", String.valueOf(changePoint));
            } else {
                params.put("expenditure_points", String.valueOf(changePoint));
                params.put("income_points", "0");
            }
            params.put("change_point", String.valueOf(changePoint));
            Member member = memberService.getById(memberPointMessage.getMemberId());
            params.put("current_point", member != null && member.getPoint() != null ? String.valueOf(member.getPoint()) : "0");
            params.put("point_time", DateUtil.toString(new Date(), DateUtil.STANDARD_FORMAT));
            NoticeMessageDTO dto = new NoticeMessageDTO();
            dto.setMemberId(memberPointMessage.getMemberId());
            dto.setNoticeMessageNodeEnum(NoticeMessageNodeEnum.POINT_CHANGE);
            dto.setParameter(params);
            dto.setPage("pages/mine/point/myPoint");
            orchestrate(dto);
        } catch (Exception e) {
            log.error("积分变更消息编排异常", e);
        }
    }

    @Override
    public void memberWithdrawal(MemberWithdrawalMessage memberWithdrawalMessage) {
        try {
            if (memberWithdrawalMessage == null || memberWithdrawalMessage.getStatus() == null) {
                return;
            }
            NoticeMessageNodeEnum nodeEnum = null;
            String auditResult = null;
            switch (WithdrawStatusEnum.valueOf(memberWithdrawalMessage.getStatus())) {
                case VIA_AUDITING:
                    nodeEnum = NoticeMessageNodeEnum.WALLET_WITHDRAWAL_AUDIT_ERROR;
                    auditResult = "审核通过";
                    break;
                case FAIL_AUDITING:
                    nodeEnum = NoticeMessageNodeEnum.WALLET_WITHDRAWAL_AUDIT_ERROR;
                    auditResult = "审核未通过";
                    break;
                case ERROR:
                    nodeEnum = NoticeMessageNodeEnum.WALLET_WITHDRAWAL_ERROR;
                    break;
                default:
                    break;
            }
            if (nodeEnum == null) {
                return;
            }
            String price = memberWithdrawalMessage.getPrice() != null
                    ? memberWithdrawalMessage.getPrice().toString() : "0.00";
            Map<String, String> params = new HashMap<>(8);
            params.put("price", price);
            params.put("withdraw_amount", price);
            if (auditResult != null) {
                params.put("audit_result", auditResult);
                params.put("withdraw_result", auditResult);
            }
            params.put("apply_time", DateUtil.toString(new Date(), DateUtil.STANDARD_FORMAT));
            NoticeMessageDTO dto = new NoticeMessageDTO();
            dto.setMemberId(memberWithdrawalMessage.getMemberId());
            dto.setNoticeMessageNodeEnum(nodeEnum);
            dto.setParameter(params);
            dto.setPage("pages/mine/deposit/withdrawApply");
            orchestrate(dto);
        } catch (Exception e) {
            log.error("提现消息编排异常", e);
        }
    }

    private void orchestrate(NoticeMessageDTO dto) {
        if (dto.getNoticeMessageNodeEnum() != null) {
            memberMessageOrchestrator.orchestrate(dto);
        }
    }
}
