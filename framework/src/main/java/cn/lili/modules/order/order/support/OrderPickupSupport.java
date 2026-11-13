package cn.lili.modules.order.order.support;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.aop.OrderLogPoint;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * 自提及虚拟订单按订单号核销，供直接提货和扫码确认共用，避免为触发切面全局查找订单服务。
 * 订单协作通过调用参数传入，不注入订单服务；核销日志与事务由本组件代理统一处理。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class OrderPickupSupport {

    /**
     * 校验原有订单类型、待核销状态和核销码，任一条件不满足时禁止提货。
     *
     * @param order 待核销订单
     * @param verificationCode 本次提交的核销码
     * @throws ServiceException 订单不存在或不符合原核销条件时抛出
     */
    public void checkVerificationOrder(Order order, String verificationCode) {
        if (order == null) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        if (!OrderTypeEnum.VIRTUAL.name().equals(order.getOrderType())
                && !DeliveryMethodEnum.SELF_PICK_UP.name().equals(order.getDeliveryMethod())) {
            throw new ServiceException(ResultCode.ORDER_TAKE_ERROR);
        }
        if (!OrderStatusEnum.TAKE.name().equals(order.getOrderStatus())
                && !OrderStatusEnum.STAY_PICKED_UP.name().equals(order.getOrderStatus())) {
            throw new ServiceException(ResultCode.ORDER_TAKE_ERROR);
        }
        if (!verificationCode.equals(order.getVerificationCode())) {
            throw new ServiceException(ResultCode.ORDER_TAKE_ERROR);
        }
    }

    /**
     * 读取并校验订单后沿用原完成流程，核销日志保持原文案，只由本组件记录一次。
     * 加入调用方事务，无事务时开启事务；校验或完成处理异常继续抛出以触发回滚。
     *
     * @param orderSn 订单编号
     * @param verificationCode 提交的核销码
     * @param orderService 提供订单读取和完成协作的调用方，不作为注入依赖
     * @return 核销后的订单实体
     */
    @OrderLogPoint(description = "'订单['+#orderSn+']提货，核销码['+#verificationCode+']'", orderSn = "#orderSn")
    @Transactional(rollbackFor = Exception.class)
    public Order take(String orderSn, String verificationCode, OrderService orderService) {
        Order order = orderService.getBySn(orderSn);
        checkVerificationOrder(order, verificationCode);
        order.setOrderStatus(OrderStatusEnum.COMPLETED.name());
        orderService.complete(orderSn);
        return order;
    }
}
