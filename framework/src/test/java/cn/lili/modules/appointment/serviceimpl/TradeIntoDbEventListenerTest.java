package cn.lili.modules.appointment.serviceimpl;

import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.event.TradeIntoDbEvent;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;

/**
 * 交易单落库领域事件监听单元测试
 * <p>
 * 验证购物车交易单落库后发布的 TradeIntoDbEvent 能正确驱动预约扩展表暂存
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@ExtendWith(MockitoExtension.class)
class TradeIntoDbEventListenerTest {

    @Spy
    @InjectMocks
    private AppointmentServiceImpl appointmentService;

    @Test
    @DisplayName("测试交易单落库事件触发预约商品结算暂存")
    void testOnTradeIntoDb() {
        TradeDTO tradeDTO = new TradeDTO();
        tradeDTO.setSn("T_TEST_001");
        TradeIntoDbEvent event = new TradeIntoDbEvent(this, tradeDTO);

        doNothing().when(appointmentService).stashCheckoutForPayment(tradeDTO);

        appointmentService.onTradeIntoDb(event);

        verify(appointmentService).stashCheckoutForPayment(tradeDTO);
    }
}
