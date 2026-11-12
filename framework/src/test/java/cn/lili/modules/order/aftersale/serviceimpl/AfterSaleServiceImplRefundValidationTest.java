package cn.lili.modules.order.aftersale.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.aftersale.entity.dto.AfterSaleDTO;
import cn.lili.modules.order.aftersale.mapper.AfterSaleMapper;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.enums.AfterSaleRefundWayEnum;
import cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum;
import cn.lili.modules.order.trade.entity.enums.AfterSaleTypeEnum;
import cn.lili.modules.payment.kit.RefundSupport;
import cn.lili.modules.promotion.service.PickupCardOrderService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * 售后申请数量下限、累计退款金额上限与平台退款状态校验测试
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class AfterSaleServiceImplRefundValidationTest {

    private AfterSaleServiceImpl afterSaleService;
    private OrderItemService orderItemService;
    private OrderService orderService;
    private AfterSaleMapper afterSaleMapper;
    private RefundSupport refundSupport;

    /**
     * 初始化 MyBatis-Plus 实体表元数据与雪花算法生成器
     */
    @BeforeAll
    static void initMybatisPlus() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, AfterSale.class);
        TableInfoHelper.initTableInfo(assistant, OrderItem.class);
        TableInfoHelper.initTableInfo(assistant, Order.class);
        cn.lili.common.utils.SnowFlake.initialize(1, 1);
    }

    /**
     * 初始化测试上下文及各依赖 mock 对象
     */
    @BeforeEach
    void setUp() {
        afterSaleService = spy(new AfterSaleServiceImpl());
        orderItemService = mock(OrderItemService.class);
        orderService = mock(OrderService.class);
        afterSaleMapper = mock(AfterSaleMapper.class);
        refundSupport = mock(RefundSupport.class);

        ReflectionTestUtils.setField(afterSaleService, "orderItemService", orderItemService);
        ReflectionTestUtils.setField(afterSaleService, "orderService", orderService);
        ReflectionTestUtils.setField(afterSaleService, "baseMapper", afterSaleMapper);
        ReflectionTestUtils.setField(afterSaleService, "refundSupport", refundSupport);
        ReflectionTestUtils.setField(afterSaleService, "appointmentService", mock(AppointmentService.class));
        ReflectionTestUtils.setField(afterSaleService, "pickupCardOrderService", mock(PickupCardOrderService.class));
        ReflectionTestUtils.setField(afterSaleService, "applicationEventPublisher", mock(ApplicationEventPublisher.class));

        doReturn(true).when(afterSaleService).save(any(AfterSale.class));
        doReturn(true).when(afterSaleService).update(any(), any());
        doNothing().when(afterSaleService).sendAfterSaleMessage(any());
    }

    /**
     * 验证买家申请售后数量必须大于0，小于等于0时拒绝申请
     */
    @Test
    void saveAfterSaleRejectsZeroOrNegativeQuantity() {
        stubOrderContext("OI001", "O001", 3, 0, 300.0);

        AfterSaleDTO dtoZero = afterSaleDto("OI001", 0);
        ServiceException exZero = assertThrows(ServiceException.class, () -> afterSaleService.saveAfterSale(dtoZero));
        assertThat(exZero.getResultCode()).isEqualTo(ResultCode.AFTER_GOODS_NUMBER_ERROR);

        AfterSaleDTO dtoNegative = afterSaleDto("OI001", -1);
        ServiceException exNeg = assertThrows(ServiceException.class, () -> afterSaleService.saveAfterSale(dtoNegative));
        assertThat(exNeg.getResultCode()).isEqualTo(ResultCode.AFTER_GOODS_NUMBER_ERROR);
    }

    /**
     * 验证申请售后数量超出订单商品剩余可退数量时拦截拒绝
     */
    @Test
    void saveAfterSaleRejectsQuantityExceedingRemaining() {
        stubOrderContext("OI001", "O001", 3, 2, 300.0);

        AfterSaleDTO dto = afterSaleDto("OI001", 2);
        ServiceException ex = assertThrows(ServiceException.class, () -> afterSaleService.saveAfterSale(dto));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.AFTER_GOODS_NUMBER_ERROR);
    }

    /**
     * 验证同一商品项历史已完成售后退款已达上限时，不可再次申请售后
     */
    @Test
    void saveAfterSaleRejectsWhenNoRemainingRefundablePrice() {
        stubOrderContext("OI001", "O001", 3, 0, 300.0);

        AfterSale priorAfterSale = new AfterSale();
        priorAfterSale.setSn("A_PRIOR");
        priorAfterSale.setOrderItemSn("OI001");
        priorAfterSale.setActualRefundPrice(300.0);
        priorAfterSale.setServiceStatus(AfterSaleStatusEnum.COMPLETE.name());
        when(afterSaleMapper.selectList(any())).thenReturn(List.of(priorAfterSale));

        AfterSaleDTO dto = afterSaleDto("OI001", 1);
        ServiceException ex = assertThrows(ServiceException.class, () -> afterSaleService.saveAfterSale(dto));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.AFTER_SALES_PRICE_ERROR);
    }

    /**
     * 验证退最后一件商品时自动倒扣历史已退金额，消除分摊舍入误差
     */
    @Test
    void saveAfterSaleCalculatesRemainingPriceForLastGoods() {
        stubOrderContext("OI001", "O001", 2, 1, 100.0);

        AfterSale priorAfterSale = new AfterSale();
        priorAfterSale.setSn("A_PRIOR");
        priorAfterSale.setOrderItemSn("OI001");
        priorAfterSale.setActualRefundPrice(40.0);
        priorAfterSale.setServiceStatus(AfterSaleStatusEnum.COMPLETE.name());
        when(afterSaleMapper.selectList(any())).thenReturn(List.of(priorAfterSale));

        AuthUser authUser = new AuthUser();
        authUser.setId("member-1");
        authUser.setNickName("member");
        authUser.setUsername("member");

        try (MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class);
             MockedStatic<OperationalJudgment> judgment = mockStatic(OperationalJudgment.class)) {
            userContextMock.when(UserContext::getCurrentUser).thenReturn(authUser);
            Order order = orderService.getBySn("O001");
            judgment.when(() -> OperationalJudgment.judgment(any(Order.class))).thenReturn(order);

            AfterSaleDTO dto = afterSaleDto("OI001", 1);
            AfterSale created = afterSaleService.saveAfterSale(dto);
            assertThat(created.getApplyRefundPrice()).isEqualTo(60.0);
        }
    }

    /**
     * 验证商家审核售后时实际退款金额不能为负数
     */
    @Test
    void reviewRejectsNegativeActualRefundPrice() {
        AfterSale current = new AfterSale();
        current.setSn("A001");
        current.setOrderSn("O001");
        current.setOrderItemSn("OI001");
        current.setFlowPrice(100.0);
        current.setServiceStatus(AfterSaleStatusEnum.APPLY.name());

        try (MockedStatic<OperationalJudgment> judgment = mockStatic(OperationalJudgment.class)) {
            judgment.when(() -> OperationalJudgment.judgment(any())).thenReturn(current);
            when(afterSaleMapper.selectOne(any())).thenReturn(current);

            ServiceException ex = assertThrows(ServiceException.class,
                    () -> afterSaleService.review("A001", AfterSaleStatusEnum.PASS.name(), "ok", -10.0));
            assertThat(ex.getResultCode()).isEqualTo(ResultCode.AFTER_SALES_PRICE_ERROR);
        }
    }

    /**
     * 验证商家审核售后时累计退款金额不可超过该商品实付总额
     */
    @Test
    void reviewRejectsCumulativeRefundExceedingItemLimit() {
        AfterSale current = new AfterSale();
        current.setSn("A002");
        current.setOrderSn("O001");
        current.setOrderItemSn("OI001");
        current.setFlowPrice(100.0);
        current.setServiceStatus(AfterSaleStatusEnum.APPLY.name());
        current.setServiceType(AfterSaleTypeEnum.RETURN_MONEY.name());
        current.setRefundWay(AfterSaleRefundWayEnum.ORIGINAL.name());

        AfterSale prior = new AfterSale();
        prior.setSn("A001");
        prior.setOrderSn("O001");
        prior.setOrderItemSn("OI001");
        prior.setActualRefundPrice(60.0);
        prior.setServiceStatus(AfterSaleStatusEnum.COMPLETE.name());

        when(afterSaleMapper.selectList(any())).thenReturn(List.of(prior));

        try (MockedStatic<OperationalJudgment> judgment = mockStatic(OperationalJudgment.class)) {
            judgment.when(() -> OperationalJudgment.judgment(any())).thenReturn(current);
            when(afterSaleMapper.selectOne(any())).thenReturn(current);

            Order order = new Order();
            order.setSn("O001");
            order.setFlowPrice(100.0);
            when(orderService.getBySn("O001")).thenReturn(order);

            OrderItem orderItem = new OrderItem();
            orderItem.setSn("OI001");
            orderItem.setOrderSn("O001");
            orderItem.setReturnGoodsNumber(1);
            orderItem.setNum(2);
            orderItem.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.PART_AFTER_SALE.name());
            when(orderItemService.getOne(any())).thenReturn(orderItem);

            ServiceException ex = assertThrows(ServiceException.class,
                    () -> afterSaleService.review("A002", AfterSaleStatusEnum.PASS.name(), "ok", 45.0));
            assertThat(ex.getResultCode()).isEqualTo(ResultCode.AFTER_SALES_PRICE_ERROR);

            assertDoesNotThrow(() -> afterSaleService.review("A002", AfterSaleStatusEnum.PASS.name(), "ok", 40.0));
        }
    }

    /**
     * 验证平台端退款操作校验当前状态：仅允许 WAIT_REFUND 状态，其它状态（如已完成 COMPLETE、待审核 APPLY）均拒绝退款以防重复退款
     */
    @Test
    void platformRefundRejectsNonWaitRefundStatus() {
        AfterSale completedAfterSale = new AfterSale();
        completedAfterSale.setSn("AS_COMPLETE");
        completedAfterSale.setServiceStatus(AfterSaleStatusEnum.COMPLETE.name());

        doReturn(completedAfterSale).when(afterSaleService).getOne(any());

        try (MockedStatic<OperationalJudgment> judgment = mockStatic(OperationalJudgment.class)) {
            judgment.when(() -> OperationalJudgment.judgment(completedAfterSale)).thenReturn(completedAfterSale);

            ServiceException ex = assertThrows(ServiceException.class,
                    () -> afterSaleService.refund("AS_COMPLETE", "平台重复退款测试"));
            assertThat(ex.getResultCode()).isEqualTo(ResultCode.AFTER_STATUS_ERROR);
            verify(refundSupport, never()).refund(any());
        }

        AfterSale applyAfterSale = new AfterSale();
        applyAfterSale.setSn("AS_APPLY");
        applyAfterSale.setServiceStatus(AfterSaleStatusEnum.APPLY.name());

        doReturn(applyAfterSale).when(afterSaleService).getOne(any());

        try (MockedStatic<OperationalJudgment> judgment = mockStatic(OperationalJudgment.class)) {
            judgment.when(() -> OperationalJudgment.judgment(applyAfterSale)).thenReturn(applyAfterSale);

            ServiceException ex = assertThrows(ServiceException.class,
                    () -> afterSaleService.refund("AS_APPLY", "未审核单退款测试"));
            assertThat(ex.getResultCode()).isEqualTo(ResultCode.AFTER_STATUS_ERROR);
            verify(refundSupport, never()).refund(any());
        }
    }

    /**
     * 验证处于 WAIT_REFUND 状态的售后单平台端正常触发渠道退款并流转至 COMPLETE 状态
     */
    @Test
    void platformRefundSucceedsWhenStatusIsWaitRefund() {
        AfterSale waitRefundAfterSale = new AfterSale();
        waitRefundAfterSale.setSn("AS_WAIT");
        waitRefundAfterSale.setServiceStatus(AfterSaleStatusEnum.WAIT_REFUND.name());

        doReturn(waitRefundAfterSale).when(afterSaleService).getOne(any());
        doNothing().when(refundSupport).refund(waitRefundAfterSale);

        try (MockedStatic<OperationalJudgment> judgment = mockStatic(OperationalJudgment.class)) {
            judgment.when(() -> OperationalJudgment.judgment(waitRefundAfterSale)).thenReturn(waitRefundAfterSale);

            AfterSale result = afterSaleService.refund("AS_WAIT", "平台退款正常处理");
            assertThat(result.getServiceStatus()).isEqualTo(AfterSaleStatusEnum.COMPLETE.name());
            verify(refundSupport).refund(waitRefundAfterSale);
        }
    }

    /**
     * 模拟构造订单明细与订单基础信息桩数据
     *
     * @param orderItemSn        订单项编号
     * @param orderSn            订单编号
     * @param num                购买数量
     * @param returnGoodsNumber 已退商品数量
     * @param flowPrice          支付实付流水金额
     */
    private void stubOrderContext(String orderItemSn, String orderSn, int num, int returnGoodsNumber, double flowPrice) {
        OrderItem orderItem = new OrderItem();
        orderItem.setSn(orderItemSn);
        orderItem.setOrderSn(orderSn);
        orderItem.setAfterSaleStatus(returnGoodsNumber > 0 ? OrderItemAfterSaleStatusEnum.PART_AFTER_SALE.name() : OrderItemAfterSaleStatusEnum.NOT_APPLIED.name());
        orderItem.setNum(num);
        orderItem.setReturnGoodsNumber(returnGoodsNumber);
        orderItem.setFlowPrice(flowPrice);

        Order order = new Order();
        order.setSn(orderSn);
        order.setStoreId("store-1");
        order.setStoreName("store-1");
        order.setOrderType(OrderTypeEnum.NORMAL.name());
        order.setOrderStatus(OrderStatusEnum.COMPLETED.name());
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setFlowPrice(flowPrice);
        order.setMemberId("member-1");

        when(orderItemService.getBySn(orderItemSn)).thenReturn(orderItem);
        when(orderItemService.getOne(any())).thenReturn(orderItem);
        when(orderService.getBySn(orderSn)).thenReturn(order);
    }

    /**
     * 构造售后申请 DTO 辅助对象
     *
     * @param orderItemSn 订单项编号
     * @param num         申请数量
     * @return 售后申请 DTO
     */
    private AfterSaleDTO afterSaleDto(String orderItemSn, Integer num) {
        AfterSaleDTO dto = new AfterSaleDTO();
        dto.setOrderItemSn(orderItemSn);
        dto.setServiceType(AfterSaleTypeEnum.RETURN_MONEY.name());
        dto.setRefundWay(AfterSaleRefundWayEnum.ORIGINAL.name());
        dto.setNum(num);
        return dto;
    }
}
