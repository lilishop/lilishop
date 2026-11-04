package cn.lili.modules.goods.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.CardKey;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.CardKeyStatusEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.mapper.CardKeyMapper;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 部分取卡失败时整单失败，触发 voidOnSystemFailure，无部分交付。
 *
 * @author Mike
 * @date 2026-07-31
 */
class CardKeyServiceImplAllocateByOrderTest {

    private CardKeyServiceImpl cardKeyService;
    private OrderMapper orderMapper;
    private OrderItemService orderItemService;
    private GoodsSkuService goodsSkuService;
    private CardKeyMapper cardKeyMapper;

    /**
     * 初始化当前测试使用的MyBatis-Plus实体元数据。
     */
    @BeforeAll
    static void initMybatisPlusEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, CardKey.class);
        TableInfoHelper.initTableInfo(assistant, Order.class);
    }

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        cardKeyService = spy(new CardKeyServiceImpl());
        orderMapper = mock(OrderMapper.class);
        orderItemService = mock(OrderItemService.class);
        goodsSkuService = mock(GoodsSkuService.class);
        cardKeyMapper = mock(CardKeyMapper.class);
        ReflectionTestUtils.setField(cardKeyService, "orderMapper", orderMapper);
        ReflectionTestUtils.setField(cardKeyService, "orderItemService", orderItemService);
        ReflectionTestUtils.setField(cardKeyService, "goodsSkuService", goodsSkuService);
        ReflectionTestUtils.setField(cardKeyService, "baseMapper", cardKeyMapper);
    }

    /**
     * 验证购买数量为三时向订单分配并绑定三张卡密。
     */
    @Test
    void allocateByOrderBindsThreeCardsForQuantityThree() {
        stubPurchaseOrderContext(3, List.of(unusedCard("ck-1"), unusedCard("ck-2"), unusedCard("ck-3")));
        doReturn(0L).when(cardKeyService).count(any(Wrapper.class));
        doReturn(true).when(cardKeyService).updateById(any(CardKey.class));
        doNothing().when(cardKeyService).syncSkuStock(anyString());

        cardKeyService.allocateByOrder("O001");

        verify(cardKeyService, times(3)).updateById(any(CardKey.class));
        verify(cardKeyService).syncSkuStock("sku-1");
    }

    /**
     * 验证订单已分配卡密时不重复分配。
     */
    @Test
    void allocateByOrderSkipsWhenAlreadyAllocated() {
        when(orderMapper.selectForUpdateBySn("O001")).thenReturn(purchaseOrder());
        doReturn(1L).when(cardKeyService).count(any(Wrapper.class));

        cardKeyService.allocateByOrder("O001");

        verify(cardKeyMapper, never()).selectForAllocate(anyString(), any(Integer.class));
    }

    /**
     * 验证卡池不足时拒绝分配并调用系统失败作废处理。
     */
    @Test
    void allocateByOrderRejectsInsufficientPoolAndInvokesVoidOnSystemFailure() {
        stubPurchaseOrderContext(2, List.of(unusedCard("ck-1")));
        doReturn(0L).when(cardKeyService).count(any(Wrapper.class));
        doNothing().when(cardKeyService).voidOnSystemFailure("O001");

        ServiceException ex = assertThrows(ServiceException.class, () -> cardKeyService.allocateByOrder("O001"));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_POOL_INSUFFICIENT);
        verify(cardKeyService).voidOnSystemFailure("O001");
        verify(cardKeyService, never()).updateById(any(CardKey.class));
        verify(cardKeyService, never()).syncSkuStock(anyString());
    }

    /**
     * 验证分配中途更新失败时作废已处理的卡密。
     */
    @Test
    void allocateByOrderVoidsPartialProgressWhenUpdateFailsMidAllocation() {
        stubPurchaseOrderContext(2, List.of(unusedCard("ck-1"), unusedCard("ck-2")));
        doReturn(0L).when(cardKeyService).count(any(Wrapper.class));
        doReturn(true).when(cardKeyService).updateById(argThat(card -> "ck-1".equals(card.getId())));
        doThrow(new RuntimeException("update failed"))
                .when(cardKeyService).updateById(argThat(card -> "ck-2".equals(card.getId())));
        doNothing().when(cardKeyService).voidOnSystemFailure("O001");

        RuntimeException ex = assertThrows(RuntimeException.class, () -> cardKeyService.allocateByOrder("O001"));

        assertThat(ex.getMessage()).isEqualTo("update failed");
        verify(cardKeyService).voidOnSystemFailure("O001");
        verify(cardKeyService, times(2)).updateById(any(CardKey.class));
        verify(cardKeyService, never()).syncSkuStock(anyString());
    }

    /**
     * 模拟购买订单及订单项查询结果。
     *
     * @param num       增量或数量
     * @param poolCards 卡池记录
     */
    private void stubPurchaseOrderContext(int num, List<CardKey> poolCards) {
        when(orderMapper.selectForUpdateBySn("O001")).thenReturn(purchaseOrder());
        when(orderItemService.getByOrderSn("O001")).thenReturn(List.of(eCouponOrderItem(num)));
        when(goodsSkuService.getById("sku-1")).thenReturn(eCouponSku());
        when(cardKeyMapper.selectForAllocate(eq("sku-1"), eq(num))).thenReturn(poolCards);
    }

    /**
     * 构造购买类型的卡密订单。
     *
     * @return 购买类型的卡密订单
     */
    private Order purchaseOrder() {
        Order order = new Order();
        order.setSn("O001");
        order.setMemberId("member-1");
        order.setOrderType(OrderTypeEnum.E_COUPON.name());
        order.setOrderPromotionType(OrderPromotionTypeEnum.NORMAL.name());
        order.setParentOrderSn("");
        return order;
    }

    /**
     * 构造卡密订单项测试数据。
     *
     * @param num 增量或数量
     * @return 卡密订单项测试数据
     */
    private OrderItem eCouponOrderItem(int num) {
        OrderItem orderItem = new OrderItem();
        orderItem.setSn("OI001");
        orderItem.setSkuId("sku-1");
        orderItem.setNum(num);
        return orderItem;
    }

    /**
     * 构造卡密商品SKU测试数据。
     *
     * @return 卡密商品SKU测试数据
     */
    private GoodsSku eCouponSku() {
        GoodsSku goodsSku = new GoodsSku();
        goodsSku.setId("sku-1");
        goodsSku.setGoodsType(GoodsTypeEnum.E_COUPON.name());
        return goodsSku;
    }

    /**
     * 构造未使用状态的卡密记录。
     *
     * @param id 测试数据ID
     * @return 未使用状态的卡密记录
     */
    private CardKey unusedCard(String id) {
        CardKey cardKey = new CardKey();
        cardKey.setId(id);
        cardKey.setSkuId("sku-1");
        cardKey.setStatus(CardKeyStatusEnum.UNUSED.name());
        cardKey.setDeleteFlag(false);
        return cardKey;
    }
}
