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
import cn.lili.modules.store.entity.vos.StoreVO;
import cn.lili.modules.store.service.StoreService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.GoodsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.alibaba.fastjson2.JSON;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
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

    /**
     * 初始化商品实体的MyBatis-Plus元数据，支持测试中生成Lambda查询及更新条件
     */
    @BeforeAll
    static void initGoodsEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, Goods.class);
    }

    /**
     * 初始化商品服务及模拟依赖，为每个用例提供独立的店铺配置和商品数据
     */
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

        ReflectionTestUtils.setField(goodsService, "goodsSearchParamService", goodsSearchParamService);
        ReflectionTestUtils.setField(goodsService, "goodsSkuService", goodsSkuService);
        ReflectionTestUtils.setField(goodsService, "goodsGalleryService", goodsGalleryService);
        ReflectionTestUtils.setField(goodsService, "settingService", settingService);
        ReflectionTestUtils.setField(goodsService, "storeService", storeService);
        ReflectionTestUtils.setField(goodsService, "cache", cache);
        ReflectionTestUtils.setField(goodsService, "categoryService", categoryService);

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
        doReturn(true).when(goodsService).update(any(Wrapper.class));
        doNothing().when(goodsService).notifyGoodsDelete(anyList());
        doReturn(existingGoods()).when(goodsService).getById(GOODS_ID);
        doNothing().when(goodsSkuService).add(any(Goods.class), any(GoodsOperationDTO.class));
        doNothing().when(goodsSkuService).update(any(Goods.class), any(GoodsOperationDTO.class));
        doNothing().when(goodsGalleryService).add(anyList(), anyString());
        when(cache.remove(any())).thenReturn(true);
    }

    /**
     * 验证新增商品无论选择上架还是下架，均按审核开关设置审核状态并同步至SKU
     *
     * @param release      是否上架
     * @param goodsCheck   是否开启商品审核
     * @param expectedAuth 预期审核状态
     */
    @ParameterizedTest
    @CsvSource({"false,true,TOBEAUDITED", "true,true,TOBEAUDITED", "false,false,PASS", "true,false,PASS"})
    void addGoodsAppliesAuditSettingRegardlessOfRelease(boolean release, boolean goodsCheck, String expectedAuth) {
        GoodsSetting config = new GoodsSetting();
        config.setGoodsCheck(goodsCheck);
        Setting setting = new Setting();
        setting.setSettingValue(JSON.toJSONString(config));
        when(settingService.get(SettingEnum.GOODS_SETTING.name())).thenReturn(setting);
        GoodsOperationDTO dto = validDto();
        dto.setRelease(release);

        try (MockedStatic<UserContext> context = mockStatic(UserContext.class)) {
            context.when(UserContext::getCurrentUser).thenReturn(storeUser());
            goodsService.addGoods(dto);
        }

        ArgumentCaptor<Goods> saved = ArgumentCaptor.forClass(Goods.class);
        verify(goodsService).save(saved.capture());
        assertThat(saved.getValue().getAuthFlag()).isEqualTo(expectedAuth);
        assertThat(saved.getValue().getMarketEnable()).isEqualTo(release ? "UPPER" : "DOWN");
        verify(goodsSkuService).add(saved.getValue(), dto);
    }

    /**
     * 验证编辑商品无论选择上架还是下架，均重新按审核开关设置审核状态并同步至SKU
     *
     * @param release      是否上架
     * @param goodsCheck   是否开启商品审核
     * @param expectedAuth 预期审核状态
     */
    @ParameterizedTest
    @CsvSource({"false,true,TOBEAUDITED", "true,true,TOBEAUDITED", "false,false,PASS", "true,false,PASS"})
    void editGoodsAppliesAuditSettingRegardlessOfRelease(boolean release, boolean goodsCheck, String expectedAuth) {
        GoodsSetting config = new GoodsSetting();
        config.setGoodsCheck(goodsCheck);
        Setting setting = new Setting();
        setting.setSettingValue(JSON.toJSONString(config));
        when(settingService.get(SettingEnum.GOODS_SETTING.name())).thenReturn(setting);
        GoodsOperationDTO dto = validDto();
        dto.setRelease(release);

        try (MockedStatic<UserContext> context = mockStatic(UserContext.class)) {
            context.when(UserContext::getCurrentUser).thenReturn(storeUser());
            goodsService.editGoods(dto, GOODS_ID);
        }

        ArgumentCaptor<Goods> updated = ArgumentCaptor.forClass(Goods.class);
        verify(goodsService).updateById(updated.capture());
        assertThat(updated.getValue().getMarketEnable()).isEqualTo(release ? "UPPER" : "DOWN");
        assertThat(updated.getValue().getAuthFlag()).isEqualTo(expectedAuth);
        verify(goodsSkuService).update(updated.getValue(), dto);
    }

    /**
     * 验证新增商品将非空参数列表写为JSON，并使用相同内容同步参数搜索倒排数据
     */
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

    /**
     * 验证新增商品传入空参数列表时写入空JSON数组，并同步清空参数搜索倒排数据
     */
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

    /**
     * 验证编辑商品将非空参数列表写为JSON，并使用相同内容同步参数搜索倒排数据
     */
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

    /**
     * 验证编辑商品传入空参数列表时写入空JSON数组，并同步清空参数搜索倒排数据
     */
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

    /**
     * 验证编辑商品未提供参数列表时不写入参数字段，也不触发参数搜索倒排同步
     */
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

    /**
     * 验证商品详情保留不参与索引的参数，避免详情展示丢失参数信息
     */
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

        GoodsVO vo = goodsService.getGoodsVO(GOODS_ID);

        assertThat(vo.getGoodsParamsDTOList()).hasSize(1);
        assertThat(vo.getGoodsParamsDTOList().get(0).getParamName()).isEqualTo("产地");
        assertThat(vo.getGoodsParamsDTOList().get(0).getIsIndex()).isZero();
        assertThat(vo.getGoodsParamsDTOList().get(0).getParamValue()).isEqualTo("广东");
    }

    /**
     * 验证商家及管理员取消计划时只清除指定方向的时间和原因，并使旧任务及商品缓存失效
     *
     * @param direction 计划方向：UPPER定时上架，DOWN定时下架
     * @param manager   是否以管理员身份操作
     */
    @ParameterizedTest
    @CsvSource({"UPPER,false", "DOWN,false", "UPPER,true", "DOWN,true"})
    void cancelScheduleOnlyClearsRequestedDirection(String direction, boolean manager) {
        Goods goods = existingGoods();
        goods.setStoreId(STORE_ID);
        java.util.Date time = new java.util.Date(1800000000000L);
        goods.setScheduledUpperTime(time);
        goods.setScheduledDownTime(time);
        doReturn(goods).when(goodsService).getById(GOODS_ID);
        cn.lili.trigger.interfaces.TimeTrigger trigger = mock(cn.lili.trigger.interfaces.TimeTrigger.class);
        cn.lili.common.properties.RocketmqCustomProperties properties = new cn.lili.common.properties.RocketmqCustomProperties();
        properties.setPromotionTopic("promotion");
        ReflectionTestUtils.setField(goodsService, "timeTrigger", trigger);
        ReflectionTestUtils.setField(goodsService, "rocketmqCustomProperties", properties);
        try (MockedStatic<UserContext> context = mockStatic(UserContext.class)) {
            context.when(UserContext::getCurrentUser).thenReturn(storeUser());
            if (manager) context.when(UserContext::getCurrentUser).thenReturn(AuthUser.builder().role(UserEnums.MANAGER).id("manager-1").build());
            goodsService.cancelGoodsMarketSchedule(GOODS_ID,
                    cn.lili.modules.goods.entity.enums.GoodsStatusEnum.valueOf(direction));
        }
        ArgumentCaptor<com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper<Goods>> wrapper = ArgumentCaptor.forClass(com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper.class);
        verify(goodsService).update(wrapper.capture());
        String prefix = direction.equals("UPPER") ? "scheduled_upper" : "scheduled_down";
        String other = direction.equals("UPPER") ? "scheduled_down" : "scheduled_upper";
        assertThat(wrapper.getValue().getSqlSet()).contains(prefix + "_time", prefix + "_reason")
                .doesNotContain(other, "market_enable");
        verify(trigger).delete(anyString(), eq(time.getTime()), anyString(), eq("promotion"));
        verify(cache).remove(any());
        verify(goodsSkuService, never()).updateGoodsSkuStatus(any());
    }

    /**
     * 验证商家取消其他店铺的计划时被拒绝，且不更新商品或清除缓存
     */
    @Test
    void cancelScheduleRejectsAnotherStoreBeforeAnyWrite() {
        Goods goods = existingGoods();
        goods.setStoreId("other-store");
        doReturn(goods).when(goodsService).getById(GOODS_ID);
        try (MockedStatic<UserContext> context = mockStatic(UserContext.class)) {
            context.when(UserContext::getCurrentUser).thenReturn(storeUser());
            org.junit.jupiter.api.Assertions.assertThrows(cn.lili.common.exception.ServiceException.class,
                    () -> goodsService.cancelGoodsMarketSchedule(GOODS_ID, cn.lili.modules.goods.entity.enums.GoodsStatusEnum.UPPER));
        }
        verify(goodsService, never()).update(any(Wrapper.class));
        verify(cache, never()).remove(any());
    }

    /**
     * 构造包含基础信息、相册和库存的商品操作测试数据
     *
     * @return 商品操作参数
     */
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

    /**
     * 构造参与搜索索引的商品参数
     *
     * @param name  参数名称
     * @param value 参数值
     * @return 参与索引的商品参数
     */
    private GoodsParamsItemDTO indexedItem(String name, String value) {
        GoodsParamsItemDTO item = new GoodsParamsItemDTO();
        item.setParamId("p-" + name);
        item.setParamName(name);
        item.setParamValue(value);
        item.setIsIndex(1);
        item.setSort(1);
        return item;
    }

    /**
     * 构造编辑及详情查询使用的已有商品
     *
     * @return 已有商品测试数据
     */
    private Goods existingGoods() {
        Goods goods = new Goods();
        goods.setId(GOODS_ID);
        goods.setGoodsName("existing");
        goods.setGoodsType(GoodsTypeEnum.VIRTUAL_GOODS.name());
        goods.setCategoryPath("cat-1");
        return goods;
    }

    /**
     * 构造归属于测试店铺的商家登录用户
     *
     * @return 商家登录用户
     */
    private AuthUser storeUser() {
        return AuthUser.builder()
                .id("clerk-1")
                .storeId(STORE_ID)
                .role(UserEnums.STORE)
                .username("merchant")
                .build();
    }

    /**
     * 构造非自营测试店铺详情，用于商品保存时读取店铺信息及审核配置
     *
     * @return 店铺详情测试数据
     */
    private StoreVO buildStoreDetail() {
        StoreVO store = new StoreVO();
        store.setId(STORE_ID);
        store.setStoreName(STORE_NAME);
        store.setSelfOperated(false);
        return store;
    }
}
