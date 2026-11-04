package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.CardKey;
import cn.lili.modules.goods.entity.enums.CardKeyStatusEnum;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import java.util.Date;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * 发卡失败系统退款后，已写 ALLOCATED 的卡须 VOIDED（不回收到 UNUSED）。
 *
 * @author Mike
 * @date 2026-07-31
 */
class CardKeyServiceImplVoidOnSystemFailureTest {

    private CardKeyServiceImpl cardKeyService;

    /**
     * 初始化当前测试使用的MyBatis-Plus实体元数据。
     */
    @BeforeAll
    static void initMybatisPlusEntityCache() {
        TableInfoHelper.initTableInfo(
                new MapperBuilderAssistant(new MybatisConfiguration(), ""),
                CardKey.class);
    }

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        cardKeyService = spy(new CardKeyServiceImpl());
    }

    /**
     * 验证系统失败时作废已分配卡密并清除订单绑定。
     */
    @Test
    void voidOnSystemFailureVoidsAllocatedCardsAndClearsOrderBinding() {
        CardKey card1 = allocatedCard("ck-1", "sku-1", "O001", "OI001");
        CardKey card2 = allocatedCard("ck-2", "sku-1", "O001", "OI001");
        doReturn(List.of(card1, card2)).when(cardKeyService).list(any(Wrapper.class));
        doReturn(true).when(cardKeyService).update(any(LambdaUpdateWrapper.class));
        doNothing().when(cardKeyService).syncSkuStock("sku-1");

        cardKeyService.voidOnSystemFailure("O001");

        ArgumentCaptor<LambdaUpdateWrapper<CardKey>> captor =
                ArgumentCaptor.forClass(LambdaUpdateWrapper.class);
        verify(cardKeyService, times(2)).update(captor.capture());
        verify(cardKeyService).syncSkuStock("sku-1");

        for (LambdaUpdateWrapper<CardKey> wrapper : captor.getAllValues()) {
            String sqlSet = wrapper.getSqlSet();
            assertThat(sqlSet)
                    .contains("status=")
                    .contains("order_sn=")
                    .contains("order_item_sn=")
                    .contains("member_id=")
                    .contains("allocated_time=");
        }
    }

    /**
     * 验证系统失败清理后按不同SKU分别同步库存。
     */
    @Test
    void voidOnSystemFailureSyncsStockForEachDistinctSku() {
        CardKey card1 = allocatedCard("ck-1", "sku-a", "O002", "OI001");
        CardKey card2 = allocatedCard("ck-2", "sku-b", "O002", "OI002");
        doReturn(List.of(card1, card2)).when(cardKeyService).list(any(Wrapper.class));
        doReturn(true).when(cardKeyService).update(any(LambdaUpdateWrapper.class));
        doNothing().when(cardKeyService).syncSkuStock(anyString());

        cardKeyService.voidOnSystemFailure("O002");

        verify(cardKeyService).syncSkuStock("sku-a");
        verify(cardKeyService).syncSkuStock("sku-b");
    }

    /**
     * 验证没有已分配卡密时系统失败清理不执行更新。
     */
    @Test
    void voidOnSystemFailureNoOpWhenNoAllocatedCards() {
        doReturn(List.of()).when(cardKeyService).list(any(Wrapper.class));

        cardKeyService.voidOnSystemFailure("O003");

        verify(cardKeyService, never()).update(any(LambdaUpdateWrapper.class));
        verify(cardKeyService, never()).syncSkuStock(anyString());
    }

    /**
     * 验证系统失败清理不影响其他订单的卡密。
     */
    @Test
    void voidOnSystemFailureDoesNotTouchUnrelatedOrders() {
        CardKey allocated = allocatedCard("ck-1", "sku-1", "O004", "OI001");
        doReturn(List.of(allocated)).when(cardKeyService).list(any(Wrapper.class));
        doReturn(true).when(cardKeyService).update(any(LambdaUpdateWrapper.class));
        doNothing().when(cardKeyService).syncSkuStock("sku-1");

        cardKeyService.voidOnSystemFailure("O004");

        verify(cardKeyService, times(1)).update(any(LambdaUpdateWrapper.class));
        verify(cardKeyService, never()).syncSkuStock("sku-other");
    }

    /**
     * 构造已分配并绑定订单的卡密记录。
     *
     * @param id          测试数据ID
     * @param skuId       SKU ID
     * @param orderSn     订单编号
     * @param orderItemSn 订单项编号
     * @return 已分配并绑定订单的卡密记录
     */
    private CardKey allocatedCard(String id, String skuId, String orderSn, String orderItemSn) {
        CardKey cardKey = new CardKey();
        cardKey.setId(id);
        cardKey.setSkuId(skuId);
        cardKey.setGoodsId("goods-1");
        cardKey.setStoreId("store-1");
        cardKey.setCardNo("CARD-" + id);
        cardKey.setCardSecret("cipher");
        cardKey.setStatus(CardKeyStatusEnum.ALLOCATED.name());
        cardKey.setOrderSn(orderSn);
        cardKey.setOrderItemSn(orderItemSn);
        cardKey.setMemberId("member-1");
        cardKey.setAllocatedTime(new Date());
        cardKey.setDeleteFlag(false);
        return cardKey;
    }
}
