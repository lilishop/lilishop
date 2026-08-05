package cn.lili.modules.order.order.serviceimpl;

import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.promotion.entity.dos.Pintuan;
import cn.lili.modules.promotion.service.PintuanService;
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
    private CardKeyService cardKeyService;
    private PintuanService pintuanService;

    @BeforeEach
    void setUp() {
        orderService = spy(new OrderServiceImpl());
        cardKeyService = mock(CardKeyService.class);
        pintuanService = mock(PintuanService.class);
        ReflectionTestUtils.setField(orderService, "cardKeyService", cardKeyService);
        ReflectionTestUtils.setField(orderService, "pintuanService", pintuanService);
    }

    @Test
    void pintuanOrderSuccessConfirmsReservationForECoupon() {
        Order order = new Order();
        order.setSn("PT-E001");
        order.setOrderType(OrderTypeEnum.E_COUPON.name());

        orderService.pintuanOrderSuccess(List.of(order));

        verify(cardKeyService).confirmReservationAndComplete("PT-E001");
    }

    @Test
    void agglomeratePintuanOrderFailedReleasesReservationAndRefundsECoupon() {
        Pintuan pintuan = new Pintuan();
        pintuan.setFictitious(false);
        pintuan.setRequiredNum(2);
        when(pintuanService.getById("p1")).thenReturn(pintuan);

        Order order = pintuanECouponOrder("PT-F001", "PT-PARENT");
        doReturn(List.of(order)).when(orderService).list(any(LambdaQueryWrapper.class));
        doReturn(true).when(orderService).systemCancelECoupon(
                eq("PT-F001"), eq("拼团人数不足，拼团失败！"), eq(true));

        orderService.agglomeratePintuanOrder("p1", "PT-PARENT");

        verify(cardKeyService).releaseReservation("PT-F001");
        verify(orderService).systemCancelECoupon("PT-F001", "拼团人数不足，拼团失败！", true);
    }

    private Order pintuanECouponOrder(String sn, String parentOrderSn) {
        Order order = new Order();
        order.setSn(sn);
        order.setParentOrderSn(parentOrderSn);
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.PINTUAN.name());
        return order;
    }
}
