package cn.lili.modules.goods.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.goods.service.GoodsSkuService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * TS-25 / S-05：E_COUPON 下架保留卡池；删除才软删（与 TS-32 边界对照）。
 *
 * @author Mike
 * @date 2026-08-03
 */
class GoodsServiceImplDelistECouponTest {

    private static final String STORE_ID = "store-1";
    private static final String GOODS_ID = "goods-ecoupon-1";
    private static final String SKU_ID = "sku-ecoupon-1";

    private GoodsServiceImpl goodsService;
    private CardKeyService cardKeyService;
    private GoodsSkuService goodsSkuService;
    private Cache<GoodsVO> cache;
    private ApplicationEventPublisher applicationEventPublisher;

    @BeforeAll
    static void initGoodsEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, Goods.class);
    }

    @BeforeEach
    void setUp() {
        goodsService = spy(new GoodsServiceImpl());
        cardKeyService = mock(CardKeyService.class);
        goodsSkuService = mock(GoodsSkuService.class);
        cache = mock(Cache.class);
        applicationEventPublisher = mock(ApplicationEventPublisher.class);

        RocketmqCustomProperties rocketmqCustomProperties = new RocketmqCustomProperties();
        rocketmqCustomProperties.setGoodsTopic("goods-topic");

        ReflectionTestUtils.setField(goodsService, "cardKeyService", cardKeyService);
        ReflectionTestUtils.setField(goodsService, "goodsSkuService", goodsSkuService);
        ReflectionTestUtils.setField(goodsService, "cache", cache);
        ReflectionTestUtils.setField(goodsService, "applicationEventPublisher", applicationEventPublisher);
        ReflectionTestUtils.setField(goodsService, "rocketmqCustomProperties", rocketmqCustomProperties);
    }

    /** 下架（DOWN）不触发 softDeletePoolOnSkuRemove。 */
    @Test
    void updateGoodsMarketAbleDownDoesNotSoftDeleteCardPool() {
        Goods goods = eCouponGoods();
        stubMarketAbleUpdate(List.of(goods));

        withStoreUser(() -> goodsService.updateGoodsMarketAble(
                List.of(GOODS_ID), GoodsStatusEnum.DOWN, "商家下架"));

        verify(cardKeyService, never()).softDeletePoolOnSkuRemove(any());
        verify(goodsSkuService).updateGoodsSkuStatusByGoodsIds(List.of(GOODS_ID), GoodsStatusEnum.DOWN.name());
    }

    /** 重新上架（UPPER）不触发 softDeletePoolOnSkuRemove。 */
    @Test
    void updateGoodsMarketAbleUpperDoesNotSoftDeleteCardPool() {
        Goods goods = eCouponGoods();
        stubMarketAbleUpdate(List.of(goods));

        withStoreUser(() -> goodsService.updateGoodsMarketAble(
                List.of(GOODS_ID), GoodsStatusEnum.UPPER, ""));

        verify(cardKeyService, never()).softDeletePoolOnSkuRemove(any());
        verify(goodsSkuService).updateGoodsSkuStatusByGoodsIds(List.of(GOODS_ID), GoodsStatusEnum.UPPER.name());
    }

    /** 删除 E_COUPON 商品时软删该 SKU 卡池。 */
    @Test
    void deleteGoodsSoftDeletesCardPoolForECoupon() {
        Goods goods = eCouponGoods();
        GoodsSku sku = new GoodsSku();
        sku.setId(SKU_ID);
        sku.setGoodsId(GOODS_ID);

        doReturn(true).when(goodsService).update(any());
        doReturn(List.of(goods)).when(goodsService).list(any(Wrapper.class));
        doNothing().when(goodsSkuService).updateGoodsSkuStatus(goods);
        when(goodsSkuService.getGoodsSkuListByGoodsId(GOODS_ID)).thenReturn(List.of(sku));

        withStoreUser(() -> goodsService.deleteGoods(List.of(GOODS_ID)));

        verify(cardKeyService).softDeletePoolOnSkuRemove(SKU_ID);
    }

    /** 删除实物商品不调用卡池软删。 */
    @Test
    void deletePhysicalGoodsDoesNotSoftDeleteCardPool() {
        Goods goods = new Goods();
        goods.setId(GOODS_ID);
        goods.setStoreId(STORE_ID);
        goods.setGoodsType(GoodsTypeEnum.PHYSICAL_GOODS.name());

        doReturn(true).when(goodsService).update(any());
        doReturn(List.of(goods)).when(goodsService).list(any(Wrapper.class));
        doNothing().when(goodsSkuService).updateGoodsSkuStatus(goods);

        withStoreUser(() -> goodsService.deleteGoods(List.of(GOODS_ID)));

        verify(cardKeyService, never()).softDeletePoolOnSkuRemove(any());
    }

    private void stubMarketAbleUpdate(List<Goods> goodsList) {
        doReturn(true).when(goodsService).update(any());
        doNothing().when(goodsSkuService).updateGoodsSkuStatusByGoodsIds(any(), any());
    }

    private Goods eCouponGoods() {
        Goods goods = new Goods();
        goods.setId(GOODS_ID);
        goods.setStoreId(STORE_ID);
        goods.setGoodsType(GoodsTypeEnum.E_COUPON.name());
        return goods;
    }

    private void withStoreUser(Runnable action) {
        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(storeUser());
            action.run();
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
}
