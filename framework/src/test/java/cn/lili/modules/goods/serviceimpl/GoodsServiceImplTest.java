package cn.lili.modules.goods.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsGallery;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.dto.GoodsParamsItemDTO;
import cn.lili.modules.goods.entity.enums.GoodsSalesModeEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsGalleryService;
import cn.lili.modules.goods.service.GoodsSearchParamService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.goods.service.WholesaleService;
import cn.lili.modules.store.entity.vos.StoreVO;
import cn.lili.modules.store.service.StoreService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.GoodsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.alibaba.fastjson2.JSON;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.MockedStatic;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * params 三态契约与 getGoodsVO 参数保真。
 *
 * @author Mike
 * @date 2026-09-03
 */
class GoodsServiceImplTest {

    private static final String STORE_ID = "store-1";
    private static final String STORE_NAME = "Test Store";
    private static final String GOODS_ID = "goods-1";

    private GoodsServiceImpl goodsService;
    private GoodsSearchParamService goodsSearchParamService;
    private GoodsSkuService goodsSkuService;
    private GoodsGalleryService goodsGalleryService;
    private SettingService settingService;
    private StoreService storeService;
    private Cache<GoodsVO> cache;
    private CategoryService categoryService;
    private WholesaleService wholesaleService;

    @BeforeAll
    static void initGoodsEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, Goods.class);
    }

    @BeforeEach
    void setUp() {
        goodsService = spy(new GoodsServiceImpl());
        goodsSearchParamService = mock(GoodsSearchParamService.class);
        goodsSkuService = mock(GoodsSkuService.class);
        goodsGalleryService = mock(GoodsGalleryService.class);
        settingService = mock(SettingService.class);
        storeService = mock(StoreService.class);
        cache = mock(Cache.class);
        categoryService = mock(CategoryService.class);
        wholesaleService = mock(WholesaleService.class);

        ReflectionTestUtils.setField(goodsService, "goodsSearchParamService", goodsSearchParamService);
        ReflectionTestUtils.setField(goodsService, "goodsSkuService", goodsSkuService);
        ReflectionTestUtils.setField(goodsService, "goodsGalleryService", goodsGalleryService);
        ReflectionTestUtils.setField(goodsService, "settingService", settingService);
        ReflectionTestUtils.setField(goodsService, "storeService", storeService);
        ReflectionTestUtils.setField(goodsService, "cache", cache);
        ReflectionTestUtils.setField(goodsService, "categoryService", categoryService);
        ReflectionTestUtils.setField(goodsService, "wholesaleService", wholesaleService);

        GoodsSetting goodsSetting = new GoodsSetting();
        goodsSetting.setGoodsCheck(false);
        Setting setting = new Setting();
        setting.setSettingValue(JSON.toJSONString(goodsSetting));
        when(settingService.get(SettingEnum.GOODS_SETTING.name())).thenReturn(setting);
        when(storeService.getStoreDetail()).thenReturn(buildStoreDetail());

        GoodsGallery gallery = new GoodsGallery();
        gallery.setOriginal("https://img.example/o.png");
        gallery.setSmall("https://img.example/s.png");
        gallery.setThumbnail("https://img.example/t.png");
        when(goodsGalleryService.getGoodsGallery(anyString())).thenReturn(gallery);

        doAnswer(invocation -> {
            Goods goods = invocation.getArgument(0);
            if (goods.getId() == null) {
                goods.setId(GOODS_ID);
            }
            return true;
        }).when(goodsService).save(any(Goods.class));
        doReturn(true).when(goodsService).updateById(any(Goods.class));
        doReturn(existingGoods()).when(goodsService).getById(GOODS_ID);
        doNothing().when(goodsSkuService).add(any(Goods.class), any(GoodsOperationDTO.class));
        doNothing().when(goodsSkuService).update(any(Goods.class), any(GoodsOperationDTO.class));
        doNothing().when(goodsGalleryService).add(anyList(), anyString());
        when(cache.remove(any())).thenReturn(true);
    }

    @Test
    void addGoodsWritesNonEmptyParamsJsonAndSyncsSameString() {
        GoodsParamsItemDTO item = indexedItem("内存", "8G");
        GoodsOperationDTO dto = validDto();
        dto.setGoodsParamsDTOList(List.of(item));
        String expectedJson = JSON.toJSONString(List.of(item));

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(storeUser());
            goodsService.addGoods(dto);
        }

        ArgumentCaptor<Goods> goodsCaptor = ArgumentCaptor.forClass(Goods.class);
        verify(goodsService).save(goodsCaptor.capture());
        assertThat(goodsCaptor.getValue().getParams()).isEqualTo(expectedJson);
        verify(goodsSearchParamService).syncByGoodsId(eq(GOODS_ID), eq(expectedJson));
    }

    @Test
    void addGoodsWritesEmptyArrayAndSyncsEmptyJson() {
        GoodsOperationDTO dto = validDto();
        dto.setGoodsParamsDTOList(new ArrayList<>());

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(storeUser());
            goodsService.addGoods(dto);
        }

        ArgumentCaptor<Goods> goodsCaptor = ArgumentCaptor.forClass(Goods.class);
        verify(goodsService).save(goodsCaptor.capture());
        assertThat(goodsCaptor.getValue().getParams()).isEqualTo("[]");
        verify(goodsSearchParamService).syncByGoodsId(eq(GOODS_ID), eq("[]"));
    }

    @Test
    void editGoodsWritesNonEmptyParamsJsonAndSyncsSameString() {
        GoodsParamsItemDTO item = indexedItem("颜色", "黑色");
        GoodsOperationDTO dto = validDto();
        dto.setGoodsParamsDTOList(List.of(item));
        String expectedJson = JSON.toJSONString(List.of(item));

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(storeUser());
            goodsService.editGoods(dto, GOODS_ID);
        }

        ArgumentCaptor<Goods> goodsCaptor = ArgumentCaptor.forClass(Goods.class);
        verify(goodsService).updateById(goodsCaptor.capture());
        assertThat(goodsCaptor.getValue().getParams()).isEqualTo(expectedJson);
        verify(goodsSearchParamService).syncByGoodsId(eq(GOODS_ID), eq(expectedJson));
    }

    @Test
    void editGoodsWritesEmptyArrayAndSyncsEmptyJson() {
        GoodsOperationDTO dto = validDto();
        dto.setGoodsParamsDTOList(new ArrayList<>());

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(storeUser());
            goodsService.editGoods(dto, GOODS_ID);
        }

        ArgumentCaptor<Goods> goodsCaptor = ArgumentCaptor.forClass(Goods.class);
        verify(goodsService).updateById(goodsCaptor.capture());
        assertThat(goodsCaptor.getValue().getParams()).isEqualTo("[]");
        verify(goodsSearchParamService).syncByGoodsId(eq(GOODS_ID), eq("[]"));
    }

    @Test
    void editGoodsSkipsParamsAndSyncWhenListIsNull() {
        GoodsOperationDTO dto = validDto();
        dto.setGoodsParamsDTOList(null);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(storeUser());
            goodsService.editGoods(dto, GOODS_ID);
        }

        ArgumentCaptor<Goods> goodsCaptor = ArgumentCaptor.forClass(Goods.class);
        verify(goodsService).updateById(goodsCaptor.capture());
        assertThat(goodsCaptor.getValue().getParams()).isNull();
        verify(goodsSearchParamService, never()).syncByGoodsId(any(), any());
    }

    @Test
    void getGoodsVOKeepsNonIndexParams() {
        GoodsParamsItemDTO nonIndex = new GoodsParamsItemDTO();
        nonIndex.setParamId("p0");
        nonIndex.setParamName("产地");
        nonIndex.setParamValue("广东");
        nonIndex.setIsIndex(0);
        Goods goods = existingGoods();
        goods.setParams(JSON.toJSONString(List.of(nonIndex)));
        doReturn(goods).when(goodsService).getById(GOODS_ID);
        when(cache.get(any())).thenReturn(null);
        when(goodsGalleryService.goodsGalleryList(GOODS_ID)).thenReturn(Collections.emptyList());
        when(goodsSkuService.getGoodsListByGoodsId(GOODS_ID)).thenReturn(Collections.emptyList());
        when(categoryService.getCategoryNamesByPath(any())).thenReturn(Collections.emptyList());
        when(wholesaleService.findByGoodsId(GOODS_ID)).thenReturn(Collections.emptyList());

        GoodsVO vo = goodsService.getGoodsVO(GOODS_ID);

        assertThat(vo.getGoodsParamsDTOList()).hasSize(1);
        assertThat(vo.getGoodsParamsDTOList().get(0).getParamName()).isEqualTo("产地");
        assertThat(vo.getGoodsParamsDTOList().get(0).getIsIndex()).isZero();
        assertThat(vo.getGoodsParamsDTOList().get(0).getParamValue()).isEqualTo("广东");
    }

    private GoodsOperationDTO validDto() {
        GoodsOperationDTO dto = new GoodsOperationDTO();
        dto.setGoodsName("测试商品");
        dto.setPrice(99D);
        dto.setSalesModel(GoodsSalesModeEnum.RETAIL.name());
        dto.setGoodsType(GoodsTypeEnum.VIRTUAL_GOODS.name());
        dto.setTemplateId("0");
        dto.setGoodsUnit("件");
        dto.setRelease(false);
        dto.setRecommend(false);
        dto.setGoodsGalleryList(List.of("https://img.example/o.png"));
        Map<String, Object> sku = new HashMap<>();
        sku.put("sn", "SN-1");
        sku.put("price", "99");
        sku.put("cost", "50");
        sku.put("weight", "1");
        sku.put("quantity", "10");
        dto.setSkuList(List.of(sku));
        return dto;
    }

    private GoodsParamsItemDTO indexedItem(String name, String value) {
        GoodsParamsItemDTO item = new GoodsParamsItemDTO();
        item.setParamId("p-" + name);
        item.setParamName(name);
        item.setParamValue(value);
        item.setIsIndex(1);
        item.setSort(1);
        return item;
    }

    private Goods existingGoods() {
        Goods goods = new Goods();
        goods.setId(GOODS_ID);
        goods.setGoodsName("existing");
        goods.setGoodsType(GoodsTypeEnum.VIRTUAL_GOODS.name());
        goods.setCategoryPath("cat-1");
        return goods;
    }

    private AuthUser storeUser() {
        return AuthUser.builder()
                .id("clerk-1")
                .storeId(STORE_ID)
                .role(UserEnums.STORE)
                .username("merchant")
                .build();
    }

    private StoreVO buildStoreDetail() {
        StoreVO store = new StoreVO();
        store.setId(STORE_ID);
        store.setStoreName(STORE_NAME);
        store.setSelfOperated(false);
        return store;
    }
}
