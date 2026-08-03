package cn.lili.modules.goods.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.GoodsService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * E_COUPON SKU quantity 读写规则：普通库存入口禁止手工修改。
 *
 * @author Mike
 * @date 2026-08-02
 */
@ExtendWith(MockitoExtension.class)
class GoodsSkuServiceImplECouponStockTest {

    @InjectMocks
    private GoodsSkuServiceImpl goodsSkuService;

    @Mock
    private GoodsService goodsService;

    private GoodsSkuServiceImpl serviceSpy;

    @BeforeEach
    void setUp() {
        serviceSpy = spy(goodsSkuService);
        ReflectionTestUtils.setField(serviceSpy, "goodsService", goodsService);
    }

    @Test
    void renderGoodsSkuListIgnoresManualQuantityForNewECouponSku() {
        GoodsSku sku = buildECouponSku("", 999);
        sku.setId(null);
        GoodsOperationDTO dto = GoodsOperationDTO.builder()
                .salesModel("RETAIL")
                .goodsGalleryList(new ArrayList<>())
                .build();
        ReflectionTestUtils.setField(goodsSkuService, "salesModelRenders", List.of());

        goodsSkuService.renderGoodsSkuList(List.of(sku), dto);

        assertThat(sku.getQuantity()).isZero();
    }

    @Test
    void updateStockRejectsECouponManualUpdate() {
        GoodsSku sku = buildECouponSku("sku-1", 10);
        doReturn(sku).when(serviceSpy).getGoodsSkuByIdFromCache("sku-1");

        ServiceException ex = assertThrows(ServiceException.class,
                () -> serviceSpy.updateStock("sku-1", 99));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_STOCK_MANUAL_FORBIDDEN);
    }

    @Test
    void updateStockByTypeRejectsECouponManualUpdate() {
        GoodsSku sku = buildECouponSku("sku-1", 10);
        doReturn(sku).when(serviceSpy).getGoodsSkuByIdFromCache("sku-1");

        ServiceException ex = assertThrows(ServiceException.class,
                () -> serviceSpy.updateStock("sku-1", 1, "ADD"));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_STOCK_MANUAL_FORBIDDEN);
    }

    @Test
    void updateGoodsStockUsesCardKeySyncForECoupon() {
        GoodsSku sku = buildECouponSku("sku-1", 5);
        doNothing().when(serviceSpy).updateStockFromCardKeySync("sku-1", 5);

        assertDoesNotThrow(() -> serviceSpy.updateGoodsStock(List.of(sku)));

        verify(serviceSpy).updateStockFromCardKeySync("sku-1", 5);
        verify(serviceSpy, never()).updateStock(eq("sku-1"), anyInt());
        verify(goodsService).updateStock("goods-1");
    }

    private GoodsSku buildECouponSku(String id, int quantity) {
        GoodsSku sku = new GoodsSku();
        sku.setId(id);
        sku.setGoodsId("goods-1");
        sku.setGoodsType(GoodsTypeEnum.E_COUPON.name());
        sku.setQuantity(quantity);
        return sku;
    }
}
