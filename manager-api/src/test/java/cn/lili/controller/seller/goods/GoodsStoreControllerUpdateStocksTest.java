package cn.lili.controller.seller.goods;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsSkuStockDTO;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.stock.entity.dto.StockFlowContext;
import cn.lili.modules.stock.entity.enums.StockFlowTypeEnum;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 店铺端商品库存修改接口单元测试，覆盖按增减方向调整库存、店铺归属隔离、参数边界校验及覆盖模式兼容。
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
@ExtendWith(MockitoExtension.class)
class GoodsStoreControllerUpdateStocksTest {

    @InjectMocks
    private GoodsStoreController controller;

    @Mock
    private GoodsService goodsService;

    @Mock
    private GoodsSkuService goodsSkuService;

    /**
     * 验证传入增减方向时路由至按类型增减库存服务，并使用服务端权威商品ID。
     */
    @Test
    @SuppressWarnings("unchecked")
    void updateStocksRoutesTypedRequestsWithServerAuthoritativeGoodsId() {
        String storeId = "store-100";
        GoodsSku sku = new GoodsSku();
        sku.setId("sku-1");
        sku.setGoodsId("server-goods-1");
        sku.setStoreId(storeId);

        GoodsSkuStockDTO dto = new GoodsSkuStockDTO();
        dto.setGoodsId("forged-goods-id");
        dto.setSkuId("sku-1");
        dto.setType("减");
        dto.setQuantity(5);

        AuthUser authUser = mock(AuthUser.class);
        when(authUser.getStoreId()).thenReturn(storeId);
        when(goodsSkuService.listByIdsAndStoreId(List.of("sku-1"), storeId)).thenReturn(List.of(sku));

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(authUser);

            controller.updateStocks(List.of(dto));

            ArgumentCaptor<List<GoodsSkuStockDTO>> captor = ArgumentCaptor.forClass(List.class);
            ArgumentCaptor<StockFlowContext> contextCaptor = ArgumentCaptor.forClass(StockFlowContext.class);
            verify(goodsSkuService).updateStocksByType(captor.capture(), contextCaptor.capture());
            verify(goodsSkuService, never()).updateStocks(anyList(), any());

            List<GoodsSkuStockDTO> captured = captor.getValue();
            assertThat(captured).hasSize(1);
            assertThat(captured.get(0).getGoodsId()).isEqualTo("server-goods-1");
            assertThat(captured.get(0).getType()).isEqualTo("SUB");
            assertThat(captured.get(0).getQuantity()).isEqualTo(5);
            assertThat(contextCaptor.getValue().getFlowType()).isEqualTo(StockFlowTypeEnum.MANUAL_ADJUST);
        }
    }

    /**
     * 验证操作非本店商品SKU时拒绝更新并抛出商品不存在异常。
     */
    @Test
    void updateStocksRejectsCrossStoreSku() {
        String storeId = "store-100";
        GoodsSkuStockDTO dto = new GoodsSkuStockDTO();
        dto.setSkuId("other-store-sku");
        dto.setType("ADD");
        dto.setQuantity(10);

        AuthUser authUser = mock(AuthUser.class);
        when(authUser.getStoreId()).thenReturn(storeId);
        when(goodsSkuService.listByIdsAndStoreId(List.of("other-store-sku"), storeId))
                .thenReturn(Collections.emptyList());

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(authUser);

            ServiceException ex = assertThrows(ServiceException.class,
                    () -> controller.updateStocks(List.of(dto)));

            assertThat(ex.getResultCode()).isEqualTo(ResultCode.GOODS_NOT_EXIST);
            verify(goodsSkuService, never()).updateStocksByType(anyList(), any());
            verify(goodsSkuService, never()).updateStocks(anyList(), any());
        }
    }

    /**
     * 验证按增减方向更新时非正整数变更量或非法方向被拒绝。
     */
    @Test
    void updateStocksRejectsNonPositiveQuantityOrInvalidType() {
        String storeId = "store-100";
        GoodsSku sku = new GoodsSku();
        sku.setId("sku-1");
        sku.setGoodsId("goods-1");
        sku.setStoreId(storeId);

        AuthUser authUser = mock(AuthUser.class);
        when(authUser.getStoreId()).thenReturn(storeId);
        when(goodsSkuService.listByIdsAndStoreId(List.of("sku-1"), storeId)).thenReturn(List.of(sku));

        GoodsSkuStockDTO zeroQtyDto = new GoodsSkuStockDTO();
        zeroQtyDto.setSkuId("sku-1");
        zeroQtyDto.setType("ADD");
        zeroQtyDto.setQuantity(0);

        GoodsSkuStockDTO invalidTypeDto = new GoodsSkuStockDTO();
        invalidTypeDto.setSkuId("sku-1");
        invalidTypeDto.setType("INVALID");
        invalidTypeDto.setQuantity(3);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(authUser);

            ServiceException zeroEx = assertThrows(ServiceException.class,
                    () -> controller.updateStocks(List.of(zeroQtyDto)));
            assertThat(zeroEx.getResultCode()).isEqualTo(ResultCode.PARAMS_ERROR);

            ServiceException typeEx = assertThrows(ServiceException.class,
                    () -> controller.updateStocks(List.of(invalidTypeDto)));
            assertThat(typeEx.getResultCode()).isEqualTo(ResultCode.PARAMS_ERROR);

            verify(goodsSkuService, never()).updateStocksByType(anyList(), any());
            verify(goodsSkuService, never()).updateStocks(anyList(), any());
        }
    }

    /**
     * 验证未传type字段时保持原有直接设置目标库存行为。
     */
    @Test
    void updateStocksPreservesLegacyDirectModeWhenTypeAbsent() {
        String storeId = "store-100";
        GoodsSku sku = new GoodsSku();
        sku.setId("sku-1");
        sku.setGoodsId("goods-1");
        sku.setStoreId(storeId);

        GoodsSkuStockDTO dto = new GoodsSkuStockDTO();
        dto.setSkuId("sku-1");
        dto.setQuantity(25);

        AuthUser authUser = mock(AuthUser.class);
        when(authUser.getStoreId()).thenReturn(storeId);
        when(goodsSkuService.listByIdsAndStoreId(List.of("sku-1"), storeId)).thenReturn(List.of(sku));

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(authUser);

            controller.updateStocks(List.of(dto));

            ArgumentCaptor<StockFlowContext> contextCaptor = ArgumentCaptor.forClass(StockFlowContext.class);
            verify(goodsSkuService).updateStocks(eq(List.of(dto)), contextCaptor.capture());
            verify(goodsSkuService, never()).updateStocksByType(anyList(), any());
            assertThat(contextCaptor.getValue().getFlowType()).isEqualTo(StockFlowTypeEnum.MANUAL_ADJUST);
        }
    }
}
