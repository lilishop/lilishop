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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
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

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
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

    /**
     * 验证卡密商品即使传入运费模板也强制使用零模板。
     */
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

    /**
     * 验证卡密商品保留零运费模板。
     */
    @Test
    void checkGoodsKeepsTemplateIdZeroForECoupon() {
        Goods goods = newGoods(GoodsTypeEnum.E_COUPON.name(), "0");

        invokeCheckGoods(goods);

        assertThat(goods.getTemplateId()).isEqualTo("0");
    }

    /**
     * 验证虚拟商品强制使用零运费模板。
     */
    @Test
    void checkGoodsForcesTemplateIdZeroForVirtualGoods() {
        Goods goods = newGoods(GoodsTypeEnum.VIRTUAL_GOODS.name(), "456");

        invokeCheckGoods(goods);

        assertThat(goods.getTemplateId()).isEqualTo("0");
    }

    /**
     * 验证实物商品未配置运费模板时拒绝保存。
     */
    @Test
    void checkGoodsRejectsPhysicalGoodsWithoutFreightTemplate() {
        Goods goods = newGoods(GoodsTypeEnum.PHYSICAL_GOODS.name(), "0");

        ServiceException ex = assertThrows(ServiceException.class, () -> invokeCheckGoods(goods));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.PHYSICAL_GOODS_NEED_TEMP);
    }

    /**
     * 验证非商家用户不能通过商品保存校验。
     */
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

    /**
     * 验证根据服务端店铺类型选择普通店或自营店审核配置。
     *
     * @param selfOperated           是否为自营店
     * @param goodsCheck             普通店商品审核开关
     * @param selfOperatedGoodsCheck 自营店商品审核开关
     * @param expected               预期结果
     */
    @ParameterizedTest
    @CsvSource(value = {
            "true,true,true,TOBEAUDITED", "true,true,false,PASS",
            "true,false,true,TOBEAUDITED", "true,false,false,PASS",
            "false,true,true,TOBEAUDITED", "false,true,false,TOBEAUDITED",
            "false,false,true,PASS", "false,false,false,PASS",
            "true,true,null,TOBEAUDITED", "true,false,null,PASS",
            "null,true,false,TOBEAUDITED"
    }, nullValues = "null")
    void checkGoodsSelectsAuditRuleFromServerStoreType(Boolean selfOperated, boolean goodsCheck,
                                                      Boolean selfOperatedGoodsCheck, String expected) {
        GoodsSetting config = new GoodsSetting();
        config.setGoodsCheck(goodsCheck);
        config.setSelfOperatedGoodsCheck(selfOperatedGoodsCheck);
        Setting setting = new Setting();
        setting.setSettingValue(JSON.toJSONString(config));
        when(settingService.get(SettingEnum.GOODS_SETTING.name())).thenReturn(setting);
        StoreVO store = buildStoreDetail();
        store.setSelfOperated(selfOperated);
        when(storeService.getStoreDetail()).thenReturn(store);
        Goods goods = newGoods(GoodsTypeEnum.E_COUPON.name(), "0");
        // 请求里的店铺类型不能覆盖服务端店铺类型。
        goods.setSelfOperated(!Boolean.TRUE.equals(selfOperated));

        invokeCheckGoods(goods);

        assertThat(goods.getAuthFlag()).isEqualTo(expected);
        assertThat(goods.getSelfOperated()).isEqualTo(selfOperated);
    }

    /**
     * 在模拟商家上下文中调用商品内部校验方法。
     *
     * @param goods 商品测试数据
     */
    private void invokeCheckGoods(Goods goods) {
        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(storeUser());
            ReflectionTestUtils.invokeMethod(goodsService, "checkGoods", goods);
        }
    }

    /**
     * 构造指定商品类型及运费模板的商品。
     *
     * @param goodsType  商品类型
     * @param templateId 消息模板ID
     * @return 指定商品类型及运费模板的商品
     */
    private Goods newGoods(String goodsType, String templateId) {
        Goods goods = new Goods();
        goods.setGoodsType(goodsType);
        goods.setTemplateId(templateId);
        goods.setGoodsName("test-goods");
        return goods;
    }

    /**
     * 构造测试店铺的商家登录用户。
     *
     * @return 测试店铺的商家登录用户
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
     * 构造商品校验使用的店铺详情。
     *
     * @return 商品校验使用的店铺详情
     */
    private StoreVO buildStoreDetail() {
        StoreVO store = new StoreVO();
        store.setId(STORE_ID);
        store.setStoreName(STORE_NAME);
        store.setSelfOperated(false);
        return store;
    }
}
