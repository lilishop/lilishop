package cn.lili.modules.order.order.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dos.OrderPackage;
import cn.lili.modules.order.order.entity.dto.OrderFulfillItemDTO;
import cn.lili.modules.order.order.entity.dto.OrderFulfillParamsDTO;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.DeliverStatusEnum;
import cn.lili.modules.order.order.entity.enums.FulfillmentModeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderPackageItemService;
import cn.lili.modules.order.order.service.OrderPackageService;
import cn.lili.modules.order.trade.service.OrderLogService;
import cn.lili.modules.system.entity.dos.Logistics;
import cn.lili.modules.system.service.LogisticsService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class OrderServiceImplDeliveryTest {

    private OrderServiceImpl orderService;
    private OrderMapper orderMapper;
    private OrderItemService orderItemService;
    private OrderPackageService orderPackageService;
    private OrderPackageItemService orderPackageItemService;
    private LogisticsService logisticsService;
    private OrderLogService orderLogService;
    private AuthUser authUser;
    private MockedStatic<UserContext> userContextMock;

    /**
     * 初始化当前测试使用的MyBatis-Plus实体元数据。
     */
    @BeforeAll
    static void initMybatisEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, Order.class);
        TableInfoHelper.initTableInfo(assistant, OrderItem.class);
        TableInfoHelper.initTableInfo(assistant, OrderPackage.class);
    }

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        orderService = spy(new OrderServiceImpl());
        orderMapper = mock(OrderMapper.class);
        orderItemService = mock(OrderItemService.class);
        orderPackageService = mock(OrderPackageService.class);
        orderPackageItemService = mock(OrderPackageItemService.class);
        logisticsService = mock(LogisticsService.class);
        orderLogService = mock(OrderLogService.class);

        ReflectionTestUtils.setField(orderService, "baseMapper", orderMapper);
        ReflectionTestUtils.setField(orderService, "orderItemService", orderItemService);
        ReflectionTestUtils.setField(orderService, "orderPackageService", orderPackageService);
        ReflectionTestUtils.setField(orderService, "orderPackageItemService", orderPackageItemService);
        ReflectionTestUtils.setField(orderService, "logisticsService", logisticsService);
        ReflectionTestUtils.setField(orderService, "orderLogService", orderLogService);
        ReflectionTestUtils.setField(orderService, "applicationEventPublisher", mock(ApplicationEventPublisher.class));
        ReflectionTestUtils.setField(orderService, "rocketmqCustomProperties", mock(RocketmqCustomProperties.class));

        authUser = new AuthUser();
        authUser.setId("store-user");
        authUser.setStoreId("store-1");
        authUser.setRole(UserEnums.STORE);
        authUser.setUsername("seller");
        userContextMock = mockStatic(UserContext.class);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(authUser);
    }

    /**
     * 释放当前用例创建的静态模拟资源。
     */
    @AfterEach
    void tearDown() {
        userContextMock.close();
    }

    /**
     * 验证整单发货创建包裹并更新已发货数量。
     */
    @Test
    void wholeDeliveryCreatesPackageAndUpdatesDeliverNumber() {
        Order order = deliverableOrder();
        OrderItem item = orderItem("item-1", 2, 0);
        Logistics logistics = logistics("logistics-1", "顺丰");

        when(orderMapper.selectForUpdateBySn("O001")).thenReturn(order);
        when(orderItemService.getByOrderSn("O001")).thenReturn(List.of(item));
        when(logisticsService.getById("logistics-1")).thenReturn(logistics);
        doReturn(true).when(orderService).updateById(any(Order.class));
        doAnswer(invocation -> null).when(orderService).sendUpdateStatusMessage(any());
        when(orderItemService.update(any())).thenReturn(true);

        OrderFulfillParamsDTO params = new OrderFulfillParamsDTO();
        params.setOrderSn("O001");
        params.setFulfillmentMode(FulfillmentModeEnum.WHOLE.name());
        params.setLogisticsId("logistics-1");
        params.setLogisticsNo("SF123");

        Order result = orderService.fulfillOrder(params);

        assertThat(result.getDeliverStatus()).isEqualTo(DeliverStatusEnum.DELIVERED.name());
        assertThat(item.getDeliverNumber()).isEqualTo(2);
        ArgumentCaptor<OrderPackage> packageCaptor = ArgumentCaptor.forClass(OrderPackage.class);
        verify(orderPackageService).save(packageCaptor.capture());
        assertThat(packageCaptor.getValue().getFulfillmentMode()).isEqualTo(FulfillmentModeEnum.WHOLE.name());
        assertThat(packageCaptor.getValue().getLogisticsNo()).isEqualTo("SF123");
    }

    /**
     * 验证拆单发货默认使用剩余未发数量。
     */
    @Test
    void splitDeliveryDefaultsRemainingQuantity() {
        Order order = deliverableOrder();
        OrderItem item = orderItem("item-1", 3, 1);
        Logistics logistics = logistics("logistics-1", "顺丰");

        when(orderMapper.selectForUpdateBySn("O001")).thenReturn(order);
        when(orderItemService.getByOrderSn("O001")).thenReturn(List.of(item));
        when(logisticsService.getById("logistics-1")).thenReturn(logistics);
        doReturn(true).when(orderService).updateById(any(Order.class));

        OrderFulfillItemDTO line = new OrderFulfillItemDTO();
        line.setOrderItemId("item-1");
        line.setDeliveryNum(1);

        OrderFulfillParamsDTO params = new OrderFulfillParamsDTO();
        params.setOrderSn("O001");
        params.setFulfillmentMode(FulfillmentModeEnum.SPLIT.name());
        params.setLogisticsId("logistics-1");
        params.setLogisticsNo("SF456");
        params.setItems(List.of(line));

        orderService.fulfillOrder(params);

        assertThat(item.getDeliverNumber()).isEqualTo(2);
        assertThat(order.getOrderStatus()).isEqualTo(OrderStatusEnum.PARTS_DELIVERED.name());
    }

    /**
     * 验证无需物流发货允许空备注并清除订单物流信息。
     */
    @Test
    void noDeliveryAllowsBlankRemarkAndClearsOrderLogistics() {
        Order order = deliverableOrder();
        OrderItem item = orderItem("item-1", 1, 0);

        when(orderMapper.selectForUpdateBySn("O001")).thenReturn(order);
        when(orderItemService.getByOrderSn("O001")).thenReturn(List.of(item));
        doReturn(true).when(orderService).updateById(any(Order.class));
        doAnswer(invocation -> null).when(orderService).sendUpdateStatusMessage(any());
        when(orderItemService.update(any())).thenReturn(true);

        OrderFulfillParamsDTO params = new OrderFulfillParamsDTO();
        params.setOrderSn("O001");
        params.setFulfillmentMode(FulfillmentModeEnum.NO_DELIVERY.name());

        orderService.fulfillOrder(params);

        assertThat(order.getLogisticsNo()).isNull();
        ArgumentCaptor<OrderPackage> packageCaptor = ArgumentCaptor.forClass(OrderPackage.class);
        verify(orderPackageService).save(packageCaptor.capture());
        assertThat(packageCaptor.getValue().getFulfillmentMode()).isEqualTo(FulfillmentModeEnum.NO_DELIVERY.name());
        assertThat(packageCaptor.getValue().getDeliveryRemark()).isNull();
    }

    /**
     * 验证部分发货订单拒绝整单发货及无需物流发货。
     */
    @Test
    void partsDeliveredRejectsWholeAndNoDelivery() {
        Order order = deliverableOrder();
        order.setOrderStatus(OrderStatusEnum.PARTS_DELIVERED.name());
        order.setDeliverStatus(DeliverStatusEnum.PARTS_DELIVERED.name());
        when(orderMapper.selectForUpdateBySn("O001")).thenReturn(order);

        OrderFulfillParamsDTO whole = new OrderFulfillParamsDTO();
        whole.setOrderSn("O001");
        whole.setFulfillmentMode(FulfillmentModeEnum.WHOLE.name());
        whole.setLogisticsId("logistics-1");
        whole.setLogisticsNo("SF123");

        assertThatThrownBy(() -> orderService.fulfillOrder(whole))
                .isInstanceOf(ServiceException.class)
                .extracting(ex -> ((ServiceException) ex).getResultCode())
                .isEqualTo(ResultCode.ORDER_DELIVER_ERROR);

        OrderFulfillParamsDTO noDelivery = new OrderFulfillParamsDTO();
        noDelivery.setOrderSn("O001");
        noDelivery.setFulfillmentMode(FulfillmentModeEnum.NO_DELIVERY.name());

        assertThatThrownBy(() -> orderService.fulfillOrder(noDelivery))
                .isInstanceOf(ServiceException.class)
                .extracting(ex -> ((ServiceException) ex).getResultCode())
                .isEqualTo(ResultCode.ORDER_DELIVER_ERROR);
    }

    /**
     * 构造可执行发货流程的普通订单。
     *
     * @return 可执行发货流程的普通订单
     */
    private Order deliverableOrder() {
        Order order = new Order();
        order.setSn("O001");
        order.setStoreId("store-1");
        order.setOrderType(OrderTypeEnum.NORMAL.name());
        order.setDeliveryMethod(DeliveryMethodEnum.LOGISTICS.name());
        order.setOrderStatus(OrderStatusEnum.UNDELIVERED.name());
        order.setDeliverStatus(DeliverStatusEnum.UNDELIVERED.name());
        return order;
    }

    /**
     * 构造订单项测试数据。
     *
     * @param id        测试数据ID
     * @param num       增量或数量
     * @param delivered 是否已发货
     * @return 订单项测试数据
     */
    private OrderItem orderItem(String id, int num, int delivered) {
        OrderItem item = new OrderItem();
        item.setId(id);
        item.setSn("OI-" + id);
        item.setOrderSn("O001");
        item.setNum(num);
        item.setDeliverNumber(delivered);
        item.setGoodsName("测试商品");
        item.setImage("img.png");
        return item;
    }

    /**
     * 构造指定ID及名称的物流公司数据。
     *
     * @param id   测试数据ID
     * @param name 名称
     * @return 指定ID及名称的物流公司数据
     */
    private Logistics logistics(String id, String name) {
        Logistics logistics = new Logistics();
        logistics.setId(id);
        logistics.setName(name);
        logistics.setCode("SF");
        return logistics;
    }
}
