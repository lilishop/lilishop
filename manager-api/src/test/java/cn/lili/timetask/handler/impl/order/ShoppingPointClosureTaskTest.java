package cn.lili.timetask.handler.impl.order;

import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.system.entity.dto.OrderSetting;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.Mockito.*;

/**
 * 验证关闭售后入口只更新售后状态，积分交由后续交易完结入口处理。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
class ShoppingPointClosureTaskTest {
    /**
     * 保留关闭入口的职责，不再扫描或结算购物积分。
     */
    @Test
    void closesEntriesWithoutPointScanning() {
        var task = new OrderEveryDayTaskExecute();
        var items = mock(OrderItemService.class);
        ReflectionTestUtils.setField(task, "orderItemService", items);
        var setting = new OrderSetting();
        setting.setCloseAfterSale(7);
        task.closeAfterSale(setting);
        verify(items).expiredAfterSaleStatus(any());
        verifyNoMoreInteractions(items);
    }
}
