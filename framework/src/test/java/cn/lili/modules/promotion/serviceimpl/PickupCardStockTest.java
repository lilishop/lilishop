package cn.lili.modules.promotion.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderGiftCardRecord;
import cn.lili.modules.order.order.mapper.OrderGiftCardRecordMapper;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.service.OrderService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * 提货库存单元测试：库存不足、数据库失败补偿、提交不补偿和取消重复通知。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
class PickupCardStockTest {
    private PickupCardServiceImpl service;
    private GoodsSkuService skus;
    private StringRedisTemplate redis;
    private DefaultRedisScript<Boolean> script;
    private TradeDTO trade;
    private OrderGiftCardRecordMapper records;

    /**
     * 创建库存模拟依赖与事务回调容器，不连接 Redis 或数据库。
     */
    @BeforeEach
    @SuppressWarnings("unchecked")
    void setUp() {
        cn.lili.common.utils.SnowFlake.initialize(1, 1);
        com.baomidou.mybatisplus.core.metadata.TableInfoHelper.initTableInfo(
                new org.apache.ibatis.builder.MapperBuilderAssistant(new com.baomidou.mybatisplus.core.MybatisConfiguration(), "pickup-test"), Order.class);
        service = new PickupCardServiceImpl();
        records = mock(OrderGiftCardRecordMapper.class);
        ReflectionTestUtils.setField(service, "recordMapper", records);
        skus = mock(GoodsSkuService.class);
        redis = mock(StringRedisTemplate.class);
        script = new DefaultRedisScript<>();
        ReflectionTestUtils.setField(service, "goodsSkuService", skus);
        ReflectionTestUtils.setField(service, "goodsService", mock(GoodsService.class));
        ReflectionTestUtils.setField(service, "cache", mock(Cache.class));
        ReflectionTestUtils.setField(service, "redis", redis);
        ReflectionTestUtils.setField(service, "quantityScript", script);
        GoodsSku sku = new GoodsSku();
        sku.setId("sku");
        sku.setGoodsId("goods");
        sku.setPrice(10D);
        CartSkuVO item = new CartSkuVO(sku);
        item.setNum(1);
        trade = new TradeDTO();
        trade.setSkuList(List.of(item));
        TransactionSynchronizationManager.initSynchronization();
    }

    /**
     * 清理线程事务状态，避免污染其他测试。
     */
    @AfterEach
    void clear() {
        TransactionSynchronizationManager.clearSynchronization();
    }

    /**
     * Redis 原子校验失败后不进入数据库扣减，也不注册重复补偿。
     */
    @Test
    void insufficientStockDoesNotTouchDatabase() {
        when(redis.execute(eq(script), anyList(), any(Object[].class))).thenReturn(false);
        assertThatThrownBy(() -> ReflectionTestUtils.invokeMethod(service, "reserveStock", trade)).isInstanceOf(ServiceException.class);
        verifyNoInteractions(skus);
        assertThat(TransactionSynchronizationManager.getSynchronizations()).isEmpty();
    }

    /**
     * 数据库条件扣减失败时，回滚回调精确补回一件库存。
     */
    @Test
    void databaseFailureCompensatesReservedStock() {
        when(redis.execute(eq(script), anyList(), any(Object[].class))).thenReturn(true);
        when(skus.update(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class))).thenReturn(false);
        assertThatThrownBy(() -> ReflectionTestUtils.invokeMethod(service, "reserveStock", trade)).isInstanceOf(ServiceException.class);
        TransactionSynchronizationManager.getSynchronizations().forEach(s -> s.afterCompletion(TransactionSynchronization.STATUS_ROLLED_BACK));
        verify(redis).execute(eq(script), anyList(), eq("-1"));
        verify(redis).execute(eq(script), anyList(), eq("1"));
    }

    /**
     * 成功提交不补库存，不调用会覆盖 Redis 库存和促销库存的通用改库存接口。
     */
    @Test
    void committedReservationIsNotCompensated() {
        when(redis.execute(eq(script), anyList(), any(Object[].class))).thenReturn(true);
        when(skus.update(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class))).thenReturn(true);
        ReflectionTestUtils.invokeMethod(service, "reserveStock", trade);
        TransactionSynchronizationManager.getSynchronizations().forEach(s -> s.afterCompletion(TransactionSynchronization.STATUS_COMMITTED));
        verify(redis, times(1)).execute(eq(script), anyList(), any(Object[].class));
        verify(skus, never()).updateGoodsStock(any());
    }

    /**
     * 已处理的取消通知不再次恢复库存，也不会更新卡权益。
     */
    @Test
    void repeatedCancellationDoesNotReleaseAgain() {
        OrderService orders = mock(OrderService.class);
        when(orders.update(any())).thenReturn(false);
        ReflectionTestUtils.setField(service, "orderService", orders);
        Order order = new Order();
        order.setSn("order");
        order.setOrderStatus("CANCELLED");
        OrderDetailVO detail = new OrderDetailVO();
        detail.setOrder(order);
        when(records.selectOne(any())).thenReturn(new OrderGiftCardRecord(), new OrderGiftCardRecord());
        service.releaseCancelledStock(detail);
        verifyNoInteractions(redis, skus);
    }

    /** 首次取消恢复库存并写入零金额记录；重复消息读取到该记录后不再补库存。 */
    @Test
    void cancellationUsesOriginalLedgerAndReleasesOnce() {
        OrderGiftCardRecord redemption = new OrderGiftCardRecord();
        redemption.setCredentialId("card");
        redemption.setTradeSn("trade");
        redemption.setMemberId("member");
        when(records.selectOne(any())).thenReturn(redemption, null, redemption, new OrderGiftCardRecord());
        when(records.insert(any(OrderGiftCardRecord.class))).thenReturn(1);
        when(skus.update(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class))).thenReturn(true);
        when(redis.execute(eq(script), anyList(), any(Object[].class))).thenReturn(true);
        OrderDetailVO detail = cancelledOrder();

        service.releaseCancelledStock(detail);
        service.releaseCancelledStock(detail);

        verify(skus, times(1)).update(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class));
        var marker = org.mockito.ArgumentCaptor.forClass(OrderGiftCardRecord.class);
        verify(records).insert(marker.capture());
        assertThat(marker.getValue().getBizType()).isEqualTo("PICKUP_CANCEL");
        assertThat(marker.getValue().getCredentialId()).isEqualTo("card");
        assertThat(marker.getValue().getChangeAmount()).isZero();
        assertThat(marker.getValue().getDeductAmount()).isZero();
        verify(records, never()).deleteById(any(java.io.Serializable.class));
    }

    /** 取消处理记录写入失败时抛错，事务回滚回调必须撤回 Redis 中已补回的库存。 */
    @Test
    void failedCancellationMarkerCompensatesRedisOnRollback() {
        when(records.selectOne(any())).thenReturn(new OrderGiftCardRecord(), null);
        when(records.insert(any(OrderGiftCardRecord.class))).thenReturn(0);
        when(skus.update(any(com.baomidou.mybatisplus.core.conditions.Wrapper.class))).thenReturn(true);
        when(redis.execute(eq(script), anyList(), any(Object[].class))).thenReturn(true);

        assertThatThrownBy(() -> service.releaseCancelledStock(cancelledOrder())).isInstanceOf(ServiceException.class);
        TransactionSynchronizationManager.getSynchronizations().forEach(
                callback -> callback.afterCompletion(TransactionSynchronization.STATUS_ROLLED_BACK));

        verify(redis, times(2)).execute(eq(script), anyList(), any(Object[].class));
    }

    /** 构建取消订单及一件商品明细，关系由流水提供，不在订单实体写入提货字段。 */
    private OrderDetailVO cancelledOrder() {
        Order order = new Order();
        order.setSn("order");
        order.setOrderStatus("CANCELLED");
        cn.lili.modules.order.order.entity.dos.OrderItem item = new cn.lili.modules.order.order.entity.dos.OrderItem();
        item.setSkuId("sku");
        item.setGoodsId("goods");
        item.setNum(1);
        OrderDetailVO detail = new OrderDetailVO();
        detail.setOrder(order);
        detail.setOrderItems(List.of(item));
        return detail;
    }
}
