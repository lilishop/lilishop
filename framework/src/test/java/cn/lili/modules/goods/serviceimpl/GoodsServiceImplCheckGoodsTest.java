package cn.lili.modules.goods.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.store.entity.vos.StoreVO;
import cn.lili.modules.store.service.StoreService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.GoodsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.alibaba.fastjson2.JSON;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * GoodsServiceImpl.checkGoods：E_COUPON 强制 templateId=0；实物需运费模板。
 *
 * @author Mike
 * @date 2026-08-03
 */
class GoodsServiceImplCheckGoodsTest {

    private static final String STORE_ID = "store-1";
    private static final String STORE_NAME = "Test Store";

    private GoodsServiceImpl goodsService;
    private SettingService settingService;
    private StoreService storeService;

    @BeforeEach
    void setUp() {
        goodsService = new GoodsServiceImpl();
        settingService = mock(SettingService.class);
        storeService = mock(StoreService.class);
        ReflectionTestUtils.setField(goodsService, "settingService", settingService);
        ReflectionTestUtils.setField(goodsService, "storeService", storeService);

        GoodsSetting goodsSetting = new GoodsSetting();
        goodsSetting.setGoodsCheck(false);
        Setting setting = new Setting();
        setting.setSettingValue(JSON.toJSONString(goodsSetting));
        when(settingService.get(SettingEnum.GOODS_SETTING.name())).thenReturn(setting);
        when(storeService.getStoreDetail()).thenReturn(buildStoreDetail());
    }

    @Test
    void checkGoodsForcesTemplateIdZeroForECouponWhenNonZeroProvided() {
        Goods goods = newGoods(GoodsTypeEnum.E_COUPON.name(), "123");

        invokeCheckGoods(goods);

        assertThat(goods.getTemplateId()).isEqualTo("0");
        assertThat(goods.getStoreId()).isEqualTo(STORE_ID);
        assertThat(goods.getStoreName()).isEqualTo(STORE_NAME);
        assertThat(goods.getAuthFlag()).isEqualTo(GoodsAuthEnum.PASS.name());
        assertThat(goods.getQuantity()).isZero();
    }

    @Test
    void checkGoodsKeepsTemplateIdZeroForECoupon() {
        Goods goods = newGoods(GoodsTypeEnum.E_COUPON.name(), "0");

        invokeCheckGoods(goods);

        assertThat(goods.getTemplateId()).isEqualTo("0");
    }

    @Test
    void checkGoodsForcesTemplateIdZeroForVirtualGoods() {
        Goods goods = newGoods(GoodsTypeEnum.VIRTUAL_GOODS.name(), "456");

        invokeCheckGoods(goods);

        assertThat(goods.getTemplateId()).isEqualTo("0");
    }

    @Test
    void checkGoodsRejectsPhysicalGoodsWithoutFreightTemplate() {
        Goods goods = newGoods(GoodsTypeEnum.PHYSICAL_GOODS.name(), "0");

        ServiceException ex = assertThrows(ServiceException.class, () -> invokeCheckGoods(goods));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.PHYSICAL_GOODS_NEED_TEMP);
    }

    @Test
    void checkGoodsRejectsNonStoreUser() {
        Goods goods = newGoods(GoodsTypeEnum.E_COUPON.name(), "0");

        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(
                    AuthUser.builder().id("member-1").role(UserEnums.MEMBER).username("buyer").build());

            ServiceException ex = assertThrows(ServiceException.class,
                    () -> ReflectionTestUtils.invokeMethod(goodsService, "checkGoods", goods));
            assertThat(ex.getResultCode()).isEqualTo(ResultCode.STORE_NOT_LOGIN_ERROR);
        }
    }

    private void invokeCheckGoods(Goods goods) {
        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(storeUser());
            ReflectionTestUtils.invokeMethod(goodsService, "checkGoods", goods);
        }
    }

    private Goods newGoods(String goodsType, String templateId) {
        Goods goods = new Goods();
        goods.setGoodsType(goodsType);
        goods.setTemplateId(templateId);
        goods.setGoodsName("test-goods");
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
