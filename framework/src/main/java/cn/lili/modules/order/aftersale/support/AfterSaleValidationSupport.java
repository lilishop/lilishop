package cn.lili.modules.order.aftersale.support;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.aftersale.entity.dto.AfterSaleDTO;
import cn.lili.modules.order.aftersale.mapper.AfterSaleMapper;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.enums.RefundStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.enums.AfterSaleRefundWayEnum;
import cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum;
import cn.lili.modules.order.trade.entity.enums.AfterSaleTypeEnum;
import cn.lili.modules.promotion.service.PickupCardOrderService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;
import java.util.Objects;

/**
 * 售后申请校验、退款额度计算与售后单构建支持类。
 * 统一处理电子卡券/提货卡禁用、预约订单退款限额、历史已退金额累计扣减与审单金额上限核验。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class AfterSaleValidationSupport {

    @Autowired
    private OrderService orderService;

    @Autowired
    private OrderItemService orderItemService;

    @Autowired
    private AppointmentService appointmentService;

    @Autowired
    private PickupCardOrderService pickupCardOrderService;

    public AfterSaleValidationSupport() {
    }

    public AfterSaleValidationSupport(OrderService orderService,
                                      OrderItemService orderItemService,
                                      AppointmentService appointmentService,
                                      PickupCardOrderService pickupCardOrderService) {
        this.orderService = orderService;
        this.orderItemService = orderItemService;
        this.appointmentService = appointmentService;
        this.pickupCardOrderService = pickupCardOrderService;
    }

    /**
     * 检查当前订单状态与商品项是否满足申请指定类型售后的全部前置条件。
     *
     * @param afterSaleMapper 售后持久层映射器
     * @param afterSaleDTO    售后申请参数
     */
    public void checkAfterSaleType(AfterSaleMapper afterSaleMapper, AfterSaleDTO afterSaleDTO) {
        if (null == afterSaleDTO || CharSequenceUtil.isEmpty(afterSaleDTO.getOrderItemSn())) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }

        OrderItem orderItem = orderItemService.getBySn(afterSaleDTO.getOrderItemSn());
        Order order = orderService.getBySn(orderItem.getOrderSn());
        rejectECouponAfterSale(order);
        validateAppointmentAfterSale(order);

        if (appointmentService.isAppointmentOrder(order)
                && OrderItemAfterSaleStatusEnum.NEW.name().equals(orderItem.getAfterSaleStatus())) {
            orderItem.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.NOT_APPLIED.name());
        }
        if (!orderItem.getAfterSaleStatus().equals(OrderItemAfterSaleStatusEnum.NOT_APPLIED.name())
                && !orderItem.getAfterSaleStatus().equals(OrderItemAfterSaleStatusEnum.PART_AFTER_SALE.name())) {
            throw new ServiceException(ResultCode.AFTER_SALES_BAN);
        }

        if (afterSaleDTO.getNum() == null || afterSaleDTO.getNum() <= 0) {
            throw new ServiceException(ResultCode.AFTER_GOODS_NUMBER_ERROR);
        }

        int returnedNum = orderItem.getReturnGoodsNumber() == null ? 0 : orderItem.getReturnGoodsNumber();
        if (afterSaleDTO.getNum() > (orderItem.getNum() - returnedNum)) {
            throw new ServiceException(ResultCode.AFTER_GOODS_NUMBER_ERROR);
        }

        if (OrderStatusEnum.CANCELLED.name().equals(order.getOrderStatus())
                || RefundStatusEnum.ALL_REFUND.name().equals(orderItem.getIsRefund())) {
            throw new ServiceException(ResultCode.AFTER_SALES_BAN);
        }

        double itemFlowPrice = getItemFlowPrice(orderItem);
        double alreadyRefundPrice = calculateRefundedPrice(afterSaleMapper, orderItem.getSn(), null);
        double remainingRefundPrice = Math.max(0.0, CurrencyUtil.sub(itemFlowPrice, alreadyRefundPrice));
        if (remainingRefundPrice <= 0) {
            throw new ServiceException(ResultCode.AFTER_SALES_PRICE_ERROR);
        }
        if (afterSaleDTO.getApplyRefundPrice() != null) {
            if (afterSaleDTO.getApplyRefundPrice() < 0
                    || BigDecimal.valueOf(afterSaleDTO.getApplyRefundPrice()).setScale(2, RoundingMode.HALF_UP)
                    .compareTo(BigDecimal.valueOf(remainingRefundPrice).setScale(2, RoundingMode.HALF_UP)) > 0) {
                throw new ServiceException(ResultCode.AFTER_SALES_PRICE_ERROR);
            }
        }
        if (OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            if (!AfterSaleTypeEnum.RETURN_MONEY.name().equals(afterSaleDTO.getServiceType())) {
                throw new ServiceException(ResultCode.AFTER_SALES_BAN);
            }
            double applyAmount = afterSaleDTO.getApplyRefundPrice() == null ? 0D : afterSaleDTO.getApplyRefundPrice();
            appointmentService.validateBuyerAfterSaleAmount(order.getSn(), applyAmount);
        }

        AfterSaleTypeEnum afterSaleTypeEnum = AfterSaleTypeEnum.valueOf(afterSaleDTO.getServiceType());
        switch (afterSaleTypeEnum) {
            case RETURN_MONEY:
                if (!PayStatusEnum.PAID.name().equals(order.getPayStatus())) {
                    throw new ServiceException(ResultCode.AFTER_SALES_BAN);
                }
                this.checkAfterSaleReturnMoneyParam(afterSaleDTO);
                break;
            case RETURN_GOODS:
                boolean availableStatus = CharSequenceUtil.equalsAny(order.getOrderStatus(),
                        OrderStatusEnum.DELIVERED.name(), OrderStatusEnum.COMPLETED.name());
                if (!PayStatusEnum.PAID.name().equals(order.getPayStatus()) && availableStatus) {
                    throw new ServiceException(ResultCode.AFTER_SALES_BAN);
                }
                break;
            default:
                break;
        }
    }

    /**
     * 审核通过售后单时校验实际退款金额不超过当前订单项及整笔订单的剩余可退余额。
     *
     * @param afterSaleMapper   售后持久层映射器
     * @param afterSale         待审核售后单
     * @param actualRefundPrice 审核实际退款金额
     */
    public void validateReviewRefundPrice(AfterSaleMapper afterSaleMapper, AfterSale afterSale, Double actualRefundPrice) {
        if (actualRefundPrice == null || actualRefundPrice < 0) {
            throw new ServiceException(ResultCode.AFTER_SALES_PRICE_ERROR);
        }
        double otherRefundedPrice = calculateRefundedPrice(afterSaleMapper, afterSale.getOrderItemSn(), afterSale.getSn());
        double remainingItemAllowed = Math.max(0.0, CurrencyUtil.sub(afterSale.getFlowPrice(), otherRefundedPrice));
        if (BigDecimal.valueOf(actualRefundPrice).setScale(2, RoundingMode.HALF_UP)
                .compareTo(BigDecimal.valueOf(remainingItemAllowed).setScale(2, RoundingMode.HALF_UP)) > 0) {
            throw new ServiceException(ResultCode.AFTER_SALES_PRICE_ERROR);
        }

        Order order = orderService.getBySn(afterSale.getOrderSn());
        if (order != null) {
            double orderFlowPrice = order.getFlowPrice() == null ? 0.0 : order.getFlowPrice();
            if (order.getPriceDetailDTO() != null && order.getPriceDetailDTO().getGiftCardPrice() != null) {
                orderFlowPrice = CurrencyUtil.add(orderFlowPrice, order.getPriceDetailDTO().getGiftCardPrice());
            }
            double otherOrderRefunded = calculateOrderRefundedPrice(afterSaleMapper, afterSale.getOrderSn(), afterSale.getSn());
            double remainingOrderAllowed = Math.max(0.0, CurrencyUtil.sub(orderFlowPrice, otherOrderRefunded));
            if (BigDecimal.valueOf(actualRefundPrice).setScale(2, RoundingMode.HALF_UP)
                    .compareTo(BigDecimal.valueOf(remainingOrderAllowed).setScale(2, RoundingMode.HALF_UP)) > 0) {
                throw new ServiceException(ResultCode.AFTER_SALES_PRICE_ERROR);
            }
        }
    }

    /**
     * 根据订单项信息与申请参数构建待落库的售后单实体，并精确核算申请退款金额。
     *
     * @param afterSaleMapper 售后持久层映射器
     * @param afterSaleDTO    售后申请参数
     * @return 构建完成待持久化的售后单
     */
    public AfterSale buildNewAfterSale(AfterSaleMapper afterSaleMapper, AfterSaleDTO afterSaleDTO) {
        AuthUser tokenUser = Objects.requireNonNull(UserContext.getCurrentUser());

        AfterSale afterSale = new AfterSale();
        BeanUtil.copyProperties(afterSaleDTO, afterSale);
        afterSale.setMemberId(tokenUser.getId());
        afterSale.setMemberName(tokenUser.getNickName());

        OrderItem orderItem = orderItemService.getBySn(afterSaleDTO.getOrderItemSn());
        Order order = OperationalJudgment.judgment(orderService.getBySn(orderItem.getOrderSn()));

        Double flowPrice = getItemFlowPrice(orderItem);
        afterSale.setStoreId(order.getStoreId());
        afterSale.setStoreName(order.getStoreName());
        afterSale.setGoodsImage(orderItem.getImage());
        afterSale.setGoodsName(orderItem.getGoodsName());
        afterSale.setSpecs(orderItem.getSpecs());
        afterSale.setFlowPrice(flowPrice);

        afterSale.setTradeSn(order.getTradeSn());
        afterSale.setOrderSn(order.getSn());
        afterSale.setPayOrderNo(order.getPayOrderNo());
        afterSale.setOrderItemSn(orderItem.getSn());
        afterSale.setServiceStatus(AfterSaleStatusEnum.APPLY.name());
        afterSale.setSn(SnowFlake.createStr("A"));

        if (afterSaleDTO.getImages() != null) {
            afterSale.setAfterSaleImage(afterSaleDTO.getImages());
        }

        double alreadyRefundPrice = calculateRefundedPrice(afterSaleMapper, orderItem.getSn(), null);
        double remainingRefundPrice = Math.max(0.0, CurrencyUtil.sub(flowPrice, alreadyRefundPrice));
        int returnedGoodsNum = orderItem.getReturnGoodsNumber() == null ? 0 : orderItem.getReturnGoodsNumber();
        int remainingGoodsNum = orderItem.getNum() - returnedGoodsNum;

        if (OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType()) && afterSaleDTO.getApplyRefundPrice() != null) {
            afterSale.setApplyRefundPrice(afterSaleDTO.getApplyRefundPrice());
        } else if (afterSale.getNum().equals(remainingGoodsNum)) {
            afterSale.setApplyRefundPrice(Math.min(remainingRefundPrice, flowPrice));
        } else {
            double utilPrice = CurrencyUtil.div(flowPrice, orderItem.getNum());
            double applyPrice = CurrencyUtil.mul(afterSale.getNum(), utilPrice);
            afterSale.setApplyRefundPrice(Math.min(applyPrice, remainingRefundPrice));
        }
        return afterSale;
    }

    /**
     * 电子卡券及提货卡兑换订单不支持买家售后。
     *
     * @param order 订单实体
     */
    public void rejectECouponAfterSale(Order order) {
        if (order != null && pickupCardOrderService.isPickupOrder(order.getSn())) {
            throw new ServiceException(ResultCode.AFTER_SALES_BAN, "提货卡兑换订单不支持退款或退货");
        }
        if (order != null && OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            throw new ServiceException(ResultCode.CARD_KEY_E_COUPON_NO_AFTER_SALE);
        }
    }

    /**
     * 预约订单售后校验：仅已支付且未服务完结可申请。
     *
     * @param order 订单实体
     */
    public void validateAppointmentAfterSale(Order order) {
        if (!appointmentService.isAppointmentOrder(order)) {
            return;
        }
        if (!PayStatusEnum.PAID.name().equals(order.getPayStatus())) {
            throw new ServiceException(ResultCode.APPOINTMENT_AFTER_SALE_UNPAID);
        }
        if (!appointmentService.canApplyBuyerAfterSale(order.getSn())) {
            throw new ServiceException(ResultCode.APPOINTMENT_AFTER_SALE_SERVICE_COMPLETE);
        }
    }

    /**
     * 检测线下退款账户信息完整性。
     *
     * @param afterSaleDTO 售后DTO
     */
    private void checkAfterSaleReturnMoneyParam(AfterSaleDTO afterSaleDTO) {
        if (AfterSaleRefundWayEnum.OFFLINE.name().equals(afterSaleDTO.getRefundWay())) {
            boolean emptyBankParam = CharSequenceUtil.isEmpty(afterSaleDTO.getBankDepositName())
                    || CharSequenceUtil.isEmpty(afterSaleDTO.getBankAccountName())
                    || CharSequenceUtil.isEmpty(afterSaleDTO.getBankAccountNumber());
            if (emptyBankParam) {
                throw new ServiceException(ResultCode.RETURN_MONEY_OFFLINE_BANK_ERROR);
            }
        }
    }

    /**
     * 获取订单商品项总实付口径（现金流水+礼品卡抵扣）。
     *
     * @param orderItem 订单商品项
     * @return 商品项实付总金额
     */
    public double getItemFlowPrice(OrderItem orderItem) {
        double flowPrice = orderItem.getFlowPrice() == null ? 0.0 : orderItem.getFlowPrice();
        PriceDetailDTO priceDetailDTO = orderItem.getPriceDetailDTO();
        if (priceDetailDTO != null && priceDetailDTO.getGiftCardPrice() != null) {
            flowPrice = CurrencyUtil.add(flowPrice, priceDetailDTO.getGiftCardPrice());
        }
        return flowPrice;
    }

    /**
     * 统计指定订单商品项历史有效售后的已申请/已退总金额。
     *
     * @param afterSaleMapper    售后持久层映射器
     * @param orderItemSn        订单项编号
     * @param excludeAfterSaleSn 需排除的售后单号（审单时排除自身）
     * @return 累计已占用退款金额
     */
    public double calculateRefundedPrice(AfterSaleMapper afterSaleMapper, String orderItemSn, String excludeAfterSaleSn) {
        LambdaQueryWrapper<AfterSale> queryWrapper = buildActiveAfterSaleQuery(excludeAfterSaleSn)
                .eq(AfterSale::getOrderItemSn, orderItemSn);
        return sumRefundedAmount(afterSaleMapper, queryWrapper);
    }

    /**
     * 统计整笔订单历史有效售后的已申请/已退总金额。
     *
     * @param afterSaleMapper    售后持久层映射器
     * @param orderSn            订单编号
     * @param excludeAfterSaleSn 需排除的售后单号
     * @return 整单累计已占用退款金额
     */
    public double calculateOrderRefundedPrice(AfterSaleMapper afterSaleMapper, String orderSn, String excludeAfterSaleSn) {
        LambdaQueryWrapper<AfterSale> queryWrapper = buildActiveAfterSaleQuery(excludeAfterSaleSn)
                .eq(AfterSale::getOrderSn, orderSn);
        return sumRefundedAmount(afterSaleMapper, queryWrapper);
    }

    private LambdaQueryWrapper<AfterSale> buildActiveAfterSaleQuery(String excludeAfterSaleSn) {
        LambdaQueryWrapper<AfterSale> queryWrapper = new LambdaQueryWrapper<AfterSale>()
                .in(AfterSale::getServiceStatus,
                        AfterSaleStatusEnum.APPLY.name(),
                        AfterSaleStatusEnum.PASS.name(),
                        AfterSaleStatusEnum.BUYER_RETURN.name(),
                        AfterSaleStatusEnum.SELLER_CONFIRM.name(),
                        AfterSaleStatusEnum.WAIT_REFUND.name(),
                        AfterSaleStatusEnum.COMPLETE.name());
        if (CharSequenceUtil.isNotEmpty(excludeAfterSaleSn)) {
            queryWrapper.ne(AfterSale::getSn, excludeAfterSaleSn);
        }
        return queryWrapper;
    }

    private double sumRefundedAmount(AfterSaleMapper afterSaleMapper, LambdaQueryWrapper<AfterSale> queryWrapper) {
        List<AfterSale> list = afterSaleMapper.selectList(queryWrapper);
        if (CollUtil.isEmpty(list)) {
            return 0.0;
        }
        double total = 0.0;
        for (AfterSale a : list) {
            Double price = a.getActualRefundPrice() != null ? a.getActualRefundPrice() : a.getApplyRefundPrice();
            if (price != null && price > 0) {
                total = CurrencyUtil.add(total, price);
            }
        }
        return total;
    }
}
