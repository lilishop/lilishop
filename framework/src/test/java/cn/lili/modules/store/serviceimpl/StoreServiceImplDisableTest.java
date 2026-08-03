package cn.lili.modules.store.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.member.service.ClerkService;
import cn.lili.modules.store.entity.dos.Store;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 关店仅下架商品，不软删卡池。
 *
 * @author Mike
 * @date 2026-08-02
 */
class StoreServiceImplDisableTest {

    private StoreServiceImpl storeService;
    private GoodsService goodsService;
    private Cache cache;
    private ClerkService clerkService;

    @BeforeAll
    static void initStoreEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, Store.class);
    }

    @BeforeEach
    void setUp() {
        storeService = spy(new StoreServiceImpl());
        goodsService = mock(GoodsService.class);
        cache = mock(Cache.class);
        clerkService = mock(ClerkService.class);
        ReflectionTestUtils.setField(storeService, "goodsService", goodsService);
        ReflectionTestUtils.setField(storeService, "cache", cache);
        ReflectionTestUtils.setField(storeService, "clerkService", clerkService);
    }

    @Test
    void disableUnderStoreGoodsOnly() {
        Store store = new Store();
        store.setId("store-1");
        doReturn(store).when(storeService).getById("store-1");
        doReturn(true).when(storeService).update(any(LambdaUpdateWrapper.class));
        when(clerkService.list(any(Wrapper.class))).thenReturn(List.of());

        boolean result = storeService.disable("store-1");

        assertThat(result).isTrue();
        verify(goodsService).underStoreGoods("store-1");
    }
}
