package cn.lili.modules.order.aftersale.support;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleApplyVO;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleNumVO;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleSearchParams;
import cn.lili.modules.order.aftersale.mapper.AfterSaleMapper;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.enums.RefundStatusEnum;
import cn.lili.modules.order.trade.entity.enums.AfterSaleRefundWayEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

/**
 * 售后查询与统计支持类，负责各状态售后单聚合计数及售后申请页视图组装。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class AfterSaleQuerySupport {

    /**
     * 聚合查询并组装各状态售后单统计数量。
     *
     * @param afterSaleMapper  售后持久层映射器
     * @param saleSearchParams 查询条件
     * @return 各状态数量统计视图
     */
    public AfterSaleNumVO getAfterSaleNumVO(AfterSaleMapper afterSaleMapper, AfterSaleSearchParams saleSearchParams) {
        AfterSaleNumVO afterSaleNumVO = new AfterSaleNumVO();
        QueryWrapper<AfterSale> baseWrapper = saleSearchParams.queryWrapper();

        List<Map<String, Object>> results = afterSaleMapper.selectMaps(
                baseWrapper.select(
                        "COUNT(CASE WHEN service_status = 'APPLY' THEN 1 END) as applyNum",
                        "COUNT(CASE WHEN service_status = 'PASS' THEN 1 END) as passNum",
                        "COUNT(CASE WHEN service_status = 'REFUSE' THEN 1 END) as refuseNum",
                        "COUNT(CASE WHEN service_status = 'BUYER_RETURN' THEN 1 END) as buyerReturnNum",
                        "COUNT(CASE WHEN service_status = 'SELLER_CONFIRM' THEN 1 END) as sellerConfirmNum",
                        "COUNT(CASE WHEN service_status = 'SELLER_TERMINATION' THEN 1 END) as sellerTerminationNum",
                        "COUNT(CASE WHEN service_status = 'BUYER_CANCEL' THEN 1 END) as buyerCancelNum",
                        "COUNT(CASE WHEN service_status = 'WAIT_REFUND' THEN 1 END) as waitRefundNum",
                        "COUNT(CASE WHEN service_status = 'COMPLETE' THEN 1 END) as completeNum"
                )
        );

        if (!results.isEmpty()) {
            Map<String, Object> result = results.get(0);
            afterSaleNumVO.setApplyNum(((Number) result.get("applyNum")).intValue());
            afterSaleNumVO.setPassNum(((Number) result.get("passNum")).intValue());
            afterSaleNumVO.setRefuseNum(((Number) result.get("refuseNum")).intValue());
            afterSaleNumVO.setBuyerReturnNum(((Number) result.get("buyerReturnNum")).intValue());
            afterSaleNumVO.setSellerConfirmNum(((Number) result.get("sellerConfirmNum")).intValue());
            afterSaleNumVO.setSellerTerminationNum(((Number) result.get("sellerTerminationNum")).intValue());
            afterSaleNumVO.setBuyerCancelNum(((Number) result.get("buyerCancelNum")).intValue());
            afterSaleNumVO.setWaitRefundNum(((Number) result.get("waitRefundNum")).intValue());
            afterSaleNumVO.setCompleteNum(((Number) result.get("completeNum")).intValue());
        } else {
            afterSaleNumVO.setApplyNum(0);
            afterSaleNumVO.setPassNum(0);
            afterSaleNumVO.setRefuseNum(0);
            afterSaleNumVO.setBuyerReturnNum(0);
            afterSaleNumVO.setSellerConfirmNum(0);
            afterSaleNumVO.setSellerTerminationNum(0);
            afterSaleNumVO.setBuyerCancelNum(0);
            afterSaleNumVO.setWaitRefundNum(0);
            afterSaleNumVO.setCompleteNum(0);
        }

        return afterSaleNumVO;
    }

    /**
     * 校验订单与订单项是否允许售后，并构建售后申请页展示数据。
     *
     * @param orderItem          订单商品项
     * @param order              订单实体
     * @param appointmentService 预约履约服务
     * @param validationSupport  售后校验支持类
     * @return 售后申请视图对象
     */
    public AfterSaleApplyVO buildAfterSaleApplyVO(OrderItem orderItem,
                                                  Order order,
                                                  AppointmentService appointmentService,
                                                  AfterSaleValidationSupport validationSupport) {
        AfterSaleApplyVO afterSaleApplyVO = new AfterSaleApplyVO();

        validationSupport.rejectECouponAfterSale(order);
        validationSupport.validateAppointmentAfterSale(order);

        if (appointmentService.isAppointmentOrder(order)
                && OrderItemAfterSaleStatusEnum.NEW.name().equals(orderItem.getAfterSaleStatus())) {
            orderItem.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.NOT_APPLIED.name());
        }
        if (!orderItem.getAfterSaleStatus().equals(OrderItemAfterSaleStatusEnum.NOT_APPLIED.name())
                && !orderItem.getAfterSaleStatus().equals(OrderItemAfterSaleStatusEnum.PART_AFTER_SALE.name())) {
            throw new ServiceException(ResultCode.AFTER_SALES_BAN);
        }

        if (!order.getPayStatus().equals(PayStatusEnum.PAID.name())) {
            throw new ServiceException(ResultCode.AFTER_SALES_NOT_PAY_ERROR);
        }
        if (OrderStatusEnum.CANCELLED.name().equals(order.getOrderStatus())
                || RefundStatusEnum.ALL_REFUND.name().equals(orderItem.getIsRefund())) {
            throw new ServiceException(ResultCode.AFTER_SALES_BAN);
        }

        if (order.getPaymentMethod().equals(PaymentMethodEnum.BANK_TRANSFER.name())) {
            afterSaleApplyVO.setRefundWay(AfterSaleRefundWayEnum.OFFLINE.name());
        } else {
            afterSaleApplyVO.setRefundWay(AfterSaleRefundWayEnum.ORIGINAL.name());
        }

        if (order.getOrderType().equals(OrderTypeEnum.VIRTUAL.name())
                || OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            afterSaleApplyVO.setReturnMoney(true);
            afterSaleApplyVO.setReturnGoods(false);
        } else {
            afterSaleApplyVO.setReturnMoney(true);
            afterSaleApplyVO.setReturnGoods(true);
        }

        afterSaleApplyVO.setAccountType(order.getPaymentMethod());
        if (OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            afterSaleApplyVO.setApplyRefundPrice(appointmentService.getRemainingRefundableAmount(order.getSn()));
        } else {
            afterSaleApplyVO.setApplyRefundPrice(CurrencyUtil.div(
                    CurrencyUtil.add(orderItem.getFlowPrice(),
                            orderItem.getPriceDetailDTO().getGiftCardPrice()),
                    orderItem.getNum()));
        }
        if (orderItem.getReturnGoodsNumber() != null) {
            afterSaleApplyVO.setNum(orderItem.getNum() - orderItem.getReturnGoodsNumber());
        }
        afterSaleApplyVO.setNum(orderItem.getNum());
        afterSaleApplyVO.setGoodsId(orderItem.getGoodsId());
        afterSaleApplyVO.setGoodsName(orderItem.getGoodsName());
        afterSaleApplyVO.setImage(orderItem.getImage());
        afterSaleApplyVO.setGoodsPrice(orderItem.getGoodsPrice());
        afterSaleApplyVO.setSkuId(orderItem.getSkuId());
        afterSaleApplyVO.setMemberId(order.getMemberId());
        return afterSaleApplyVO;
    }
}
