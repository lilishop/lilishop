package cn.lili.controller.seller.order;

import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.service.OrderService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class OrderStoreControllerCardKeyTest {

    @InjectMocks
    private OrderStoreController controller;

    @Mock
    private OrderService orderService;

    @Mock
    private CardKeyService cardKeyService;

    @Test
    void detailEnrichesCardKeysAfterStoreAccessCheck() {
        String orderSn = "O-STORE-001";
        Order order = new Order();
        OrderDetailVO detail = new OrderDetailVO();
        detail.setOrder(order);
        when(orderService.getBySn(orderSn)).thenReturn(order);
        when(orderService.queryDetail(orderSn)).thenReturn(detail);

        try (MockedStatic<OperationalJudgment> judgment = mockStatic(OperationalJudgment.class)) {
            judgment.when(() -> OperationalJudgment.judgment(order)).thenReturn(order);

            controller.detail(orderSn);

            judgment.verify(() -> OperationalJudgment.judgment(order));
            verify(cardKeyService).enrichOrderDetail(detail);
        }
    }

    @Test
    void detailDoesNotQueryOrEnrichWhenStoreAccessDenied() {
        String orderSn = "O-STORE-002";
        Order order = new Order();
        when(orderService.getBySn(orderSn)).thenReturn(order);

        try (MockedStatic<OperationalJudgment> judgment = mockStatic(OperationalJudgment.class)) {
            judgment.when(() -> OperationalJudgment.judgment(order))
                    .thenThrow(new ServiceException("无权访问订单"));

            assertThrows(ServiceException.class, () -> controller.detail(orderSn));

            verify(orderService, never()).queryDetail(orderSn);
            verify(cardKeyService, never()).enrichOrderDetail(any(OrderDetailVO.class));
        }
    }
}
