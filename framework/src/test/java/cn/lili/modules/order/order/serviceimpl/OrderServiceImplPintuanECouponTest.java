package cn.lili.modules.order.order.serviceimpl;

import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.promotion.entity.dos.Pintuan;
import cn.lili.modules.promotion.mapper.PintuanMapper;
import cn.lili.modules.order.order.support.OrderPintuanSupport;
import cn.lili.modules.order.order.support.ECouponOrderLifecycleSupport;
import cn.lili.modules.order.order.support.OrderStatusSupport;
import cn.lili.trigger.interfaces.TimeTrigger;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 拼团 E_COUPON 成团/失败编排（TS-41 / TS-42）。
 *
 * @author Mike
 * @date 2026-08-02
 */
class OrderServiceImplPintuanECouponTest {

    private OrderServiceImpl orderService;
    private ECouponFulfillmentService eCouponFulfillmentService;
    private ECouponOrderLifecycleSupport eCouponOrderLifecycleSupport;
    private PintuanMapper pintuanMapper;

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        orderService = spy(new OrderServiceImpl());
        ReflectionTestUtils.setField(orderService, "pickupCardOrderService", org.mockito.Mockito.mock(cn.lili.modules.promotion.service.PickupCardOrderService.class));
        eCouponFulfillmentService = mock(ECouponFulfillmentService.class);
        eCouponOrderLifecycleSupport = mock(ECouponOrderLifecycleSupport.class);
        pintuanMapper = mock(PintuanMapper.class);
        ReflectionTestUtils.setField(orderService, "orderPintuanSupport",
                new OrderPintuanSupport(pintuanMapper, mock(TimeTrigger.class), mock(RocketmqCustomProperties.class),
                        eCouponFulfillmentService, eCouponOrderLifecycleSupport, mock(OrderStatusSupport.class)));
    }

    /**
     * 验证卡密拼团成功后确认预占并完成履约。
     */
    @Test
    void pintuanOrderSuccessConfirmsReservationForECoupon() {
        Order order = new Order();
        order.setSn("PT-E001");
        order.setOrderType(OrderTypeEnum.E_COUPON.name());

        orderService.pintuanOrderSuccess(List.of(order));

        verify(eCouponFulfillmentService).confirmReservationAndComplete("PT-E001");
    }

    /**
     * 验证卡密拼团失败后释放预占并退款。
     */
    @Test
    void agglomeratePintuanOrderFailedReleasesReservationAndRefundsECoupon() {
        Pintuan pintuan = new Pintuan();
        pintuan.setFictitious(false);
        pintuan.setRequiredNum(2);
        when(pintuanMapper.selectById("p1")).thenReturn(pintuan);

        Order order = pintuanECouponOrder("PT-F001", "PT-PARENT");
        doReturn(List.of(order)).when(orderService).list(any(LambdaQueryWrapper.class));
        doReturn(true).when(eCouponOrderLifecycleSupport).cancel(
                eq("PT-F001"), eq("拼团人数不足，拼团失败！"), eq(true));

        orderService.agglomeratePintuanOrder("p1", "PT-PARENT");

        verify(eCouponFulfillmentService).releaseReservation("PT-F001");
        verify(eCouponOrderLifecycleSupport).cancel("PT-F001", "拼团人数不足，拼团失败！", true);
    }

    /**
     * 构造关联拼团主订单的卡密订单。
     *
     * @param sn            订单编号
     * @param parentOrderSn 主订单编号
     * @return 关联拼团主订单的卡密订单
     */
    private Order pintuanECouponOrder(String sn, String parentOrderSn) {
        Order order = new Order();
        order.setSn(sn);
        order.setParentOrderSn(parentOrderSn);
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.PINTUAN.name());
        return order;
    }
}
