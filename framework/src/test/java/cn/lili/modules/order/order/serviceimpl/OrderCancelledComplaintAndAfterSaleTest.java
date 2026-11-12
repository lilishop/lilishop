package cn.lili.modules.order.order.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.order.aftersale.serviceimpl.AfterSaleServiceImpl;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.OrderComplaintDTO;
import cn.lili.modules.order.order.entity.enums.OrderComplaintStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.promotion.service.PickupCardOrderService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * 已取消订单投诉与售后申请前置校验单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class OrderCancelledComplaintAndAfterSaleTest {

    private OrderComplaintServiceImpl orderComplaintService;
    private AfterSaleServiceImpl afterSaleService;
    private OrderService orderService;
    private OrderItemService orderItemService;
    private PickupCardOrderService pickupCardOrderService;
    private AppointmentService appointmentService;
    private MockedStatic<UserContext> userContextMockedStatic;

    /**
     * 初始化测试组件与模拟上下文
     */
    @BeforeEach
    void setUp() {
        orderComplaintService = new OrderComplaintServiceImpl();
        afterSaleService = new AfterSaleServiceImpl();
        orderService = mock(OrderService.class);
        orderItemService = mock(OrderItemService.class);
        pickupCardOrderService = mock(PickupCardOrderService.class);
        appointmentService = mock(AppointmentService.class);

        ReflectionTestUtils.setField(orderComplaintService, "orderService", orderService);
        ReflectionTestUtils.setField(orderComplaintService, "orderItemService", orderItemService);

        ReflectionTestUtils.setField(afterSaleService, "orderService", orderService);
        ReflectionTestUtils.setField(afterSaleService, "orderItemService", orderItemService);
        ReflectionTestUtils.setField(afterSaleService, "pickupCardOrderService", pickupCardOrderService);
        ReflectionTestUtils.setField(afterSaleService, "appointmentService", appointmentService);

        userContextMockedStatic = mockStatic(UserContext.class);
        AuthUser authUser = new AuthUser("test_user", "member_1", "test_user", "1", UserEnums.MEMBER);
        userContextMockedStatic.when(UserContext::getCurrentUser).thenReturn(authUser);
    }

    /**
     * 清理静态模拟资源
     */
    @AfterEach
    void tearDown() {
        if (userContextMockedStatic != null) {
            userContextMockedStatic.close();
        }
    }

    /**
     * 验证已取消订单发起交易投诉时被拒绝
     */
    @Test
    @DisplayName("已取消订单发起投诉应抛出COMPLAINT_ORDER_STATUS_ERROR")
    void testCancelledOrderCannotComplain() {
        String orderSn = "O123456789";
        String skuId = "SKU001";

        Order order = new Order();
        order.setSn(orderSn);
        order.setMemberId("member_1");
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        order.setPayStatus(PayStatusEnum.PAID.name());

        OrderItem orderItem = new OrderItem();
        orderItem.setOrderSn(orderSn);
        orderItem.setSkuId(skuId);
        orderItem.setComplainStatus(OrderComplaintStatusEnum.EXPIRED.name());

        OrderDetailVO orderDetailVO = new OrderDetailVO();
        orderDetailVO.setOrder(order);
        orderDetailVO.setOrderItems(Collections.singletonList(orderItem));

        when(orderService.queryDetail(orderSn)).thenReturn(orderDetailVO);

        OrderComplaintDTO complaintDTO = new OrderComplaintDTO();
        complaintDTO.setOrderSn(orderSn);
        complaintDTO.setSkuId(skuId);

        ServiceException ex = assertThrows(ServiceException.class, () -> orderComplaintService.addOrderComplain(complaintDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.COMPLAINT_ORDER_STATUS_ERROR);
    }

    /**
     * 验证未付款订单发起交易投诉时被拒绝
     */
    @Test
    @DisplayName("未付款订单发起投诉应抛出COMPLAINT_ORDER_STATUS_ERROR")
    void testUnpaidOrderCannotComplain() {
        String orderSn = "O123456788";
        String skuId = "SKU002";

        Order order = new Order();
        order.setSn(orderSn);
        order.setMemberId("member_1");
        order.setOrderStatus(OrderStatusEnum.UNPAID.name());
        order.setPayStatus(PayStatusEnum.UNPAID.name());

        OrderItem orderItem = new OrderItem();
        orderItem.setOrderSn(orderSn);
        orderItem.setSkuId(skuId);
        orderItem.setComplainStatus(OrderComplaintStatusEnum.NEW.name());

        OrderDetailVO orderDetailVO = new OrderDetailVO();
        orderDetailVO.setOrder(order);
        orderDetailVO.setOrderItems(Collections.singletonList(orderItem));

        when(orderService.queryDetail(orderSn)).thenReturn(orderDetailVO);

        OrderComplaintDTO complaintDTO = new OrderComplaintDTO();
        complaintDTO.setOrderSn(orderSn);
        complaintDTO.setSkuId(skuId);

        ServiceException ex = assertThrows(ServiceException.class, () -> orderComplaintService.addOrderComplain(complaintDTO));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.COMPLAINT_ORDER_STATUS_ERROR);
    }

    /**
     * 验证已取消订单查询售后申请信息时被拒绝
     */
    @Test
    @DisplayName("已取消订单申请售后查询应抛出AFTER_SALES_BAN")
    void testCancelledOrderCannotGetAfterSaleVO() {
        String orderSn = "O123456787";
        String orderItemSn = "OI001";

        Order order = new Order();
        order.setSn(orderSn);
        order.setMemberId("member_1");
        order.setOrderType(OrderTypeEnum.NORMAL.name());
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        order.setPayStatus(PayStatusEnum.PAID.name());

        OrderItem orderItem = new OrderItem();
        orderItem.setSn(orderItemSn);
        orderItem.setOrderSn(orderSn);
        orderItem.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.NOT_APPLIED.name());

        when(orderItemService.getBySn(orderItemSn)).thenReturn(orderItem);
        when(orderService.getBySn(orderSn)).thenReturn(order);
        when(pickupCardOrderService.isPickupOrder(orderSn)).thenReturn(false);
        when(appointmentService.isAppointmentOrder(order)).thenReturn(false);

        ServiceException ex = assertThrows(ServiceException.class, () -> afterSaleService.getAfterSaleVO(orderItemSn));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.AFTER_SALES_BAN);
    }

    /**
     * 验证未付款且已取消订单查询售后申请信息时被拒绝
     */
    @Test
    @DisplayName("未付款且已取消订单申请售后查询应抛出AFTER_SALES_NOT_PAY_ERROR")
    void testUnpaidAndCancelledOrderCannotGetAfterSaleVO() {
        String orderSn = "O123456786";
        String orderItemSn = "OI002";

        Order order = new Order();
        order.setSn(orderSn);
        order.setMemberId("member_1");
        order.setOrderType(OrderTypeEnum.NORMAL.name());
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        order.setPayStatus(PayStatusEnum.UNPAID.name());

        OrderItem orderItem = new OrderItem();
        orderItem.setSn(orderItemSn);
        orderItem.setOrderSn(orderSn);
        orderItem.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.NOT_APPLIED.name());

        when(orderItemService.getBySn(orderItemSn)).thenReturn(orderItem);
        when(orderService.getBySn(orderSn)).thenReturn(order);
        when(pickupCardOrderService.isPickupOrder(orderSn)).thenReturn(false);
        when(appointmentService.isAppointmentOrder(order)).thenReturn(false);

        ServiceException ex = assertThrows(ServiceException.class, () -> afterSaleService.getAfterSaleVO(orderItemSn));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.AFTER_SALES_NOT_PAY_ERROR);
    }
}
