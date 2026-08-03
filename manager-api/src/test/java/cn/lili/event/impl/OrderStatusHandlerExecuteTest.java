package cn.lili.event.impl;

import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.service.TradeService;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/**
 * 零元交易自动支付（含促销后 E_COUPON flowPrice=0）。
 *
 * @author Mike
 * @date 2026-08-02
 */
class OrderStatusHandlerExecuteTest {

    private OrderStatusHandlerExecute handler;
    private TradeService tradeService;

    @BeforeEach
    void setUp() {
        handler = new OrderStatusHandlerExecute();
        tradeService = mock(TradeService.class);
        ReflectionTestUtils.setField(handler, "tradeService", tradeService);
    }

    @Test
    void orderCreateAutoPaysWhenFlowPriceIsZero() {
        TradeDTO tradeDTO = new TradeDTO(CartTypeEnum.BUY_NOW);
        tradeDTO.setSn("TRADE-001");
        PriceDetailDTO priceDetail = tradeDTO.getPriceDetailDTO();
        priceDetail.setFlowPrice(0D);
        priceDetail.setGiftCardPrice(0D);

        handler.orderCreate(tradeDTO);

        verify(tradeService).payTrade("TRADE-001", PaymentMethodEnum.BANK_TRANSFER.name(), "-1");
    }
}
