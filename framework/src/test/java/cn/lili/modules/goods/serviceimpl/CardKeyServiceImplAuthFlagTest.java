package cn.lili.modules.goods.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import cn.lili.modules.store.service.StoreService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 审核拒绝禁管卡池；待审核可备货。
 *
 * @author Mike
 * @date 2026-07-31
 */
class CardKeyServiceImplAuthFlagTest {

    private static final String SKU_ID = "sku-ecoupon-1";
    private static final String GOODS_ID = "goods-1";
    private static final String STORE_ID = "store-1";

    private CardKeyServiceImpl cardKeyService;
    private GoodsSkuService goodsSkuService;
    private GoodsService goodsService;
    private StoreService storeService;

    @BeforeEach
    void setUp() {
        cardKeyService = new CardKeyServiceImpl();
        goodsSkuService = mock(GoodsSkuService.class);
        goodsService = mock(GoodsService.class);
        storeService = mock(StoreService.class);
        ReflectionTestUtils.setField(cardKeyService, "goodsSkuService", goodsSkuService);
        ReflectionTestUtils.setField(cardKeyService, "goodsService", goodsService);
        ReflectionTestUtils.setField(cardKeyService, "storeService", storeService);

        when(goodsSkuService.getById(SKU_ID)).thenReturn(buildSku());
        when(storeService.getById(STORE_ID)).thenReturn(buildOpenStore());
    }

    @Test
    void validateSkuForManageRejectsRefusedGoods() {
        when(goodsService.getById(GOODS_ID)).thenReturn(buildGoods(GoodsAuthEnum.REFUSE));

        ServiceException ex = assertThrows(ServiceException.class,
                () -> invokeValidateSkuForManage(SKU_ID, STORE_ID));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_GOODS_AUTH_REFUSE);
    }

    @ParameterizedTest
    @EnumSource(value = GoodsAuthEnum.class, names = {"TOBEAUDITED", "PASS"})
    void validateSkuForManageAllowsNonRefusedAuthFlags(GoodsAuthEnum authFlag) {
        when(goodsService.getById(GOODS_ID)).thenReturn(buildGoods(authFlag));

        GoodsSku sku = assertDoesNotThrow(() -> invokeValidateSkuForManage(SKU_ID, STORE_ID));
        assertThat(sku.getId()).isEqualTo(SKU_ID);
    }

    private GoodsSku invokeValidateSkuForManage(String skuId, String storeId) {
        return ReflectionTestUtils.invokeMethod(cardKeyService, "validateSkuForManage", skuId, storeId);
    }

    private GoodsSku buildSku() {
        GoodsSku sku = new GoodsSku();
        sku.setId(SKU_ID);
        sku.setGoodsId(GOODS_ID);
        sku.setStoreId(STORE_ID);
        sku.setGoodsType(GoodsTypeEnum.E_COUPON.name());
        sku.setDeleteFlag(false);
        return sku;
    }

    private Goods buildGoods(GoodsAuthEnum authFlag) {
        Goods goods = new Goods();
        goods.setId(GOODS_ID);
        goods.setAuthFlag(authFlag.name());
        goods.setDeleteFlag(false);
        return goods;
    }

    private Store buildOpenStore() {
        Store store = new Store();
        store.setId(STORE_ID);
        store.setStoreDisable(StoreStatusEnum.OPEN.name());
        return store;
    }
}
