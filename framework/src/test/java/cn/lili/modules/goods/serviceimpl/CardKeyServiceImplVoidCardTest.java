package cn.lili.modules.goods.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dos.CardKey;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.CardKeyStatusEnum;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import cn.lili.modules.store.service.StoreService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * 作废 UNUSED 卡密并同步库存。
 *
 * @author Mike
 * @date 2026-08-02
 */
class CardKeyServiceImplVoidCardTest {

    private static final String CARD_ID = "card-1";
    private static final String SKU_ID = "sku-1";
    private static final String STORE_ID = "store-1";

    private CardKeyServiceImpl cardKeyService;
    private GoodsSkuService goodsSkuService;
    private GoodsService goodsService;
    private StoreService storeService;

    @BeforeAll
    static void initEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, CardKey.class);
    }

    @BeforeEach
    void setUp() {
        cardKeyService = spy(new CardKeyServiceImpl());
        goodsSkuService = mock(GoodsSkuService.class);
        goodsService = mock(GoodsService.class);
        storeService = mock(StoreService.class);
        ReflectionTestUtils.setField(cardKeyService, "goodsSkuService", goodsSkuService);
        ReflectionTestUtils.setField(cardKeyService, "goodsService", goodsService);
        ReflectionTestUtils.setField(cardKeyService, "storeService", storeService);

        org.mockito.Mockito.when(goodsSkuService.getById(SKU_ID)).thenReturn(buildSku());
        org.mockito.Mockito.when(goodsService.getById("goods-1")).thenReturn(buildGoods());
        org.mockito.Mockito.when(storeService.getById(STORE_ID)).thenReturn(buildStore());
        doReturn(true).when(cardKeyService).update(any());
        doNothing().when(cardKeyService).syncSkuStock(SKU_ID);
    }

    @Test
    void voidCardMarksUnusedAsVoidedAndSyncsStock() {
        CardKey cardKey = buildUnusedCard();
        doReturn(cardKey).when(cardKeyService).getById(CARD_ID);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(storeUser());

            cardKeyService.voidCard(CARD_ID);

            verify(cardKeyService).update(any());
            verify(cardKeyService).syncSkuStock(SKU_ID);
        }
    }

    @Test
    void voidCardRejectsNonUnusedStatus() {
        CardKey cardKey = buildUnusedCard();
        cardKey.setStatus(CardKeyStatusEnum.ALLOCATED.name());
        doReturn(cardKey).when(cardKeyService).getById(CARD_ID);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(storeUser());

            ServiceException ex = assertThrows(ServiceException.class, () -> cardKeyService.voidCard(CARD_ID));
            assertThat(ex.getResultCode()).isEqualTo(ResultCode.CARD_KEY_CANNOT_VOID);
        }
    }

    private AuthUser storeUser() {
        return AuthUser.builder()
                .id("clerk-1")
                .storeId(STORE_ID)
                .role(UserEnums.STORE)
                .username("merchant")
                .build();
    }

    private CardKey buildUnusedCard() {
        CardKey cardKey = new CardKey();
        cardKey.setId(CARD_ID);
        cardKey.setSkuId(SKU_ID);
        cardKey.setStoreId(STORE_ID);
        cardKey.setStatus(CardKeyStatusEnum.UNUSED.name());
        cardKey.setDeleteFlag(false);
        return cardKey;
    }

    private GoodsSku buildSku() {
        GoodsSku sku = new GoodsSku();
        sku.setId(SKU_ID);
        sku.setGoodsId("goods-1");
        sku.setStoreId(STORE_ID);
        sku.setGoodsType(GoodsTypeEnum.E_COUPON.name());
        sku.setDeleteFlag(false);
        return sku;
    }

    private Goods buildGoods() {
        Goods goods = new Goods();
        goods.setId("goods-1");
        goods.setAuthFlag(GoodsAuthEnum.PASS.name());
        goods.setDeleteFlag(false);
        return goods;
    }

    private Store buildStore() {
        Store store = new Store();
        store.setId(STORE_ID);
        store.setStoreDisable(StoreStatusEnum.OPEN.name());
        return store;
    }
}
