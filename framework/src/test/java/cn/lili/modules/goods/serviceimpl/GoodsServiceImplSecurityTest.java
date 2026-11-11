package cn.lili.modules.goods.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.store.entity.dos.FreightTemplate;
import cn.lili.modules.store.entity.vos.StoreVO;
import cn.lili.modules.store.service.FreightTemplateService;
import cn.lili.modules.store.service.StoreService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.GoodsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * 商品服务越权防护单元测试，覆盖商家跨店铺编辑商品、批量设置运费模板及定时上下架的安全隔离校验。
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class GoodsServiceImplSecurityTest {

    private static final String CURRENT_STORE_ID = "store-current";
    private static final String OTHER_STORE_ID = "store-other";
    private static final String GOODS_ID = "goods-100";

    private GoodsServiceImpl goodsService;
    private SettingService settingService;
    private StoreService storeService;
    private FreightTemplateService freightTemplateService;
    private GoodsSkuService goodsSkuService;
    private Cache<Goods> cache;

    @BeforeAll
    static void initGoodsEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, Goods.class);
    }

    @BeforeEach
    void setUp() {
        goodsService = spy(new GoodsServiceImpl());
        settingService = mock(SettingService.class);
        storeService = mock(StoreService.class);
        freightTemplateService = mock(FreightTemplateService.class);
        goodsSkuService = mock(GoodsSkuService.class);
        cache = mock(Cache.class);

        ReflectionTestUtils.setField(goodsService, "settingService", settingService);
        ReflectionTestUtils.setField(goodsService, "storeService", storeService);
        ReflectionTestUtils.setField(goodsService, "freightTemplateService", freightTemplateService);
        ReflectionTestUtils.setField(goodsService, "goodsSkuService", goodsSkuService);
        ReflectionTestUtils.setField(goodsService, "cache", cache);

        GoodsSetting goodsSetting = new GoodsSetting();
        goodsSetting.setGoodsCheck(false);
        Setting setting = new Setting();
        setting.setSettingValue(JSON.toJSONString(goodsSetting));
        when(settingService.get(SettingEnum.GOODS_SETTING.name())).thenReturn(setting);

        StoreVO storeVO = new StoreVO();
        storeVO.setId(CURRENT_STORE_ID);
        storeVO.setStoreName("Current Store");
        storeVO.setSelfOperated(false);
        when(storeService.getStoreDetail()).thenReturn(storeVO);
    }

    private AuthUser currentStoreUser() {
        return AuthUser.builder()
                .id("user-1")
                .storeId(CURRENT_STORE_ID)
                .role(UserEnums.STORE)
                .username("merchant")
                .build();
    }

    private GoodsOperationDTO buildGoodsOperationDTO() {
        GoodsOperationDTO dto = new GoodsOperationDTO();
        dto.setGoodsName("Test Goods");
        dto.setCategoryPath("1");
        dto.setGoodsType(GoodsTypeEnum.VIRTUAL_GOODS.name());
        dto.setTemplateId("0");
        dto.setGoodsUnit("件");
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

    @Test
    @DisplayName("商家编辑其他店铺的商品时抛出越权异常")
    void editGoodsBelongingToOtherStoreThrowsAuthorityError() {
        Goods otherGoods = new Goods();
        otherGoods.setId(GOODS_ID);
        otherGoods.setStoreId(OTHER_STORE_ID);
        otherGoods.setGoodsType(GoodsTypeEnum.VIRTUAL_GOODS.name());
        otherGoods.setGoodsName("Other Goods");
        doReturn(otherGoods).when(goodsService).getById(GOODS_ID);

        GoodsOperationDTO dto = buildGoodsOperationDTO();

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(currentStoreUser());

            ServiceException exception = assertThrows(ServiceException.class,
                    () -> goodsService.editGoods(dto, GOODS_ID));

            assertThat(exception.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        }
    }

    @Test
    @DisplayName("商家批量设置运费模板时包含其他店铺商品抛出越权异常")
    void freightWithOtherStoreGoodsThrowsAuthorityError() {
        String templateId = "tpl-1";
        FreightTemplate freightTemplate = new FreightTemplate();
        freightTemplate.setId(templateId);
        freightTemplate.setStoreId(CURRENT_STORE_ID);
        when(freightTemplateService.getById(templateId)).thenReturn(freightTemplate);

        List<String> goodsIds = List.of("g-1", "g-2");
        // 模拟数据库统计仅查到1件属于当前店铺，少于请求数量2件
        doReturn(1L).when(goodsService).count(any(Wrapper.class));

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(currentStoreUser());

            ServiceException exception = assertThrows(ServiceException.class,
                    () -> goodsService.freight(goodsIds, templateId));

            assertThat(exception.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        }
    }

    @Test
    @DisplayName("商家定时上下架其他店铺商品抛出越权异常")
    void scheduleGoodsMarketWithOtherStoreGoodsThrowsAuthorityError() {
        Goods g1 = new Goods();
        g1.setId("g-1");
        g1.setStoreId(CURRENT_STORE_ID);

        Goods g2 = new Goods();
        g2.setId("g-2");
        g2.setStoreId(OTHER_STORE_ID); // 属于其他店铺

        doReturn(List.of(g1, g2)).when(goodsService).listByIds(List.of("g-1", "g-2"));

        Date future = new Date(System.currentTimeMillis() + 3600000);

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(currentStoreUser());

            ServiceException exception = assertThrows(ServiceException.class,
                    () -> goodsService.scheduleGoodsMarket(List.of("g-1", "g-2"), GoodsStatusEnum.UPPER, future, "定时上架"));

            assertThat(exception.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        }
    }
}
