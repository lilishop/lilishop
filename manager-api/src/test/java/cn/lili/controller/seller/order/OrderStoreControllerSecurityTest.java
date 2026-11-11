package cn.lili.controller.seller.order;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.service.OrderPackageService;
import cn.lili.modules.order.order.service.OrderService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 商家端订单包裹与物流踪迹查询防越权单元测试，验证跨店铺查询时通过 OperationalJudgment 阻断。
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@ExtendWith(MockitoExtension.class)
class OrderStoreControllerSecurityTest {

    private static final String CURRENT_STORE_ID = "store-current";
    private static final String OTHER_STORE_ID = "store-other";
    private static final String ORDER_SN = "SN-2001";

    @InjectMocks
    private OrderStoreController controller;

    @Mock
    private OrderService orderService;

    @Mock
    private OrderPackageService orderPackageService;

    private AuthUser currentStoreUser() {
        return AuthUser.builder()
                .id("user-1")
                .storeId(CURRENT_STORE_ID)
                .role(UserEnums.STORE)
                .username("merchant")
                .build();
    }

    @Test
    @DisplayName("商家查看其他店铺订单包裹时在Controller层被拒绝")
    void getPackageOtherStoreThrowsAuthorityError() {
        Order otherOrder = new Order();
        otherOrder.setSn(ORDER_SN);
        otherOrder.setStoreId(OTHER_STORE_ID);

        when(orderService.getBySn(ORDER_SN)).thenReturn(otherOrder);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(currentStoreUser());

            ServiceException exception = assertThrows(ServiceException.class,
                    () -> controller.getPackage(ORDER_SN));

            assertThat(exception.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
            verify(orderPackageService, never()).getOrderPackageVOList(any());
        }
    }

    @Test
    @DisplayName("商家查看本店订单包裹时正常调用业务服务")
    void getPackageOwnStoreSuccess() {
        Order ownOrder = new Order();
        ownOrder.setSn(ORDER_SN);
        ownOrder.setStoreId(CURRENT_STORE_ID);

        when(orderService.getBySn(ORDER_SN)).thenReturn(ownOrder);
        when(orderPackageService.getOrderPackageVOList(ORDER_SN)).thenReturn(Collections.emptyList());

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(currentStoreUser());

            controller.getPackage(ORDER_SN);

            verify(orderPackageService).getOrderPackageVOList(ORDER_SN);
        }
    }

    @Test
    @DisplayName("商家查看其他店铺物流踪迹列表时在Controller层被拒绝")
    void getTracesListOtherStoreThrowsAuthorityError() {
        Order otherOrder = new Order();
        otherOrder.setSn(ORDER_SN);
        otherOrder.setStoreId(OTHER_STORE_ID);

        when(orderService.getBySn(ORDER_SN)).thenReturn(otherOrder);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(currentStoreUser());

            ServiceException exception = assertThrows(ServiceException.class,
                    () -> controller.getTracesList(ORDER_SN));

            assertThat(exception.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
            verify(orderPackageService, never()).getOrderPackageVOList(any());
        }
    }

    @Test
    @DisplayName("商家查看本店物流踪迹列表时正常调用业务服务")
    void getTracesListOwnStoreSuccess() {
        Order ownOrder = new Order();
        ownOrder.setSn(ORDER_SN);
        ownOrder.setStoreId(CURRENT_STORE_ID);

        when(orderService.getBySn(ORDER_SN)).thenReturn(ownOrder);
        when(orderPackageService.getOrderPackageVOList(ORDER_SN)).thenReturn(Collections.emptyList());

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(currentStoreUser());

            controller.getTracesList(ORDER_SN);

            verify(orderPackageService).getOrderPackageVOList(ORDER_SN);
        }
    }
}
