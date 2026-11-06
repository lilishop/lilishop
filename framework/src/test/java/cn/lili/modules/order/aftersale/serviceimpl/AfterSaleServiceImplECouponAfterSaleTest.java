package cn.lili.modules.order.aftersale.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.modules.order.aftersale.entity.dto.AfterSaleDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.enums.AfterSaleTypeEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * E_COUPON 订单售后 API 须返回 CARD_KEY_E_COUPON_NO_AFTER_SALE，优先于 afterSaleStatus 校验。
 *
 * @author Mike
 * @date 2026-07-31
 */
class AfterSaleServiceImplECouponAfterSaleTest {

    private AfterSaleServiceImpl afterSaleService;
    private OrderItemService orderItemService;
    private OrderService orderService;

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        afterSaleService = new AfterSaleServiceImpl();
        ReflectionTestUtils.setField(afterSaleService, "appointmentService", mock(cn.lili.modules.appointment.service.AppointmentService.class));
        ReflectionTestUtils.setField(afterSaleService, "pickupCardOrderService", org.mockito.Mockito.mock(cn.lili.modules.promotion.service.PickupCardOrderService.class));
        orderItemService = mock(OrderItemService.class);
        orderService = mock(OrderService.class);
        ReflectionTestUtils.setField(afterSaleService, "orderItemService", orderItemService);
        ReflectionTestUtils.setField(afterSaleService, "orderService", orderService);
    }

    /**
     * 验证卡密订单申请新售后时返回专用错误码。
     */
    @Test
    void saveAfterSaleRejectsECouponWithDedicatedCodeWhenAfterSaleStatusIsNew() {
        stubOrderContext("OI001", "O001", OrderTypeEnum.E_COUPON, OrderItemAfterSaleStatusEnum.NEW.name());

        AfterSaleDTO dto = afterSaleDto("OI001");

        ServiceException ex = assertThrows(ServiceException.class, () -> afterSaleService.saveAfterSale(dto));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_E_COUPON_NO_AFTER_SALE);
    }

    /**
     * 验证查询新售后卡密订单信息时返回专用错误码。
     */
    @Test
    void getAfterSaleVORejectsECouponWithDedicatedCodeWhenAfterSaleStatusIsNew() {
        Order order = stubOrderContext("OI001", "O001", OrderTypeEnum.E_COUPON, OrderItemAfterSaleStatusEnum.NEW.name());

        try (MockedStatic<OperationalJudgment> judgment = mockStatic(OperationalJudgment.class)) {
            judgment.when(() -> OperationalJudgment.judgment(any(Order.class))).thenReturn(order);

            ServiceException ex = assertThrows(ServiceException.class, () -> afterSaleService.getAfterSaleVO("OI001"));

            assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_E_COUPON_NO_AFTER_SALE);
        }
    }

    /**
     * 验证非卡密订单仍遵循新售后状态校验。
     */
    @Test
    void saveAfterSaleStillRejectsNonECouponWhenAfterSaleStatusIsNew() {
        stubOrderContext("OI002", "O002", OrderTypeEnum.NORMAL, OrderItemAfterSaleStatusEnum.NEW.name());

        AfterSaleDTO dto = afterSaleDto("OI002");

        ServiceException ex = assertThrows(ServiceException.class, () -> afterSaleService.saveAfterSale(dto));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.AFTER_SALES_BAN);
    }

    /**
     * 模拟售后校验所需的订单及订单项上下文。
     *
     * @param orderItemSn     订单项编号
     * @param orderSn         订单编号
     * @param orderType       订单类型
     * @param afterSaleStatus 售后状态
     * @return 售后测试使用的订单项
     */
    private Order stubOrderContext(String orderItemSn, String orderSn, OrderTypeEnum orderType, String afterSaleStatus) {
        OrderItem orderItem = new OrderItem();
        orderItem.setSn(orderItemSn);
        orderItem.setOrderSn(orderSn);
        orderItem.setAfterSaleStatus(afterSaleStatus);
        orderItem.setNum(1);
        orderItem.setReturnGoodsNumber(0);

        Order order = new Order();
        order.setSn(orderSn);
        order.setOrderType(orderType.name());
        order.setOrderStatus(OrderStatusEnum.COMPLETED.name());
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setMemberId("member-1");

        when(orderItemService.getBySn(orderItemSn)).thenReturn(orderItem);
        when(orderService.getBySn(orderSn)).thenReturn(order);
        return order;
    }

    /**
     * 构造指定订单项的售后申请参数。
     *
     * @param orderItemSn 订单项编号
     * @return 指定订单项的售后申请参数
     */
    private AfterSaleDTO afterSaleDto(String orderItemSn) {
        AfterSaleDTO dto = new AfterSaleDTO();
        dto.setOrderItemSn(orderItemSn);
        dto.setServiceType(AfterSaleTypeEnum.RETURN_MONEY.name());
        dto.setNum(1);
        return dto;
    }
}
