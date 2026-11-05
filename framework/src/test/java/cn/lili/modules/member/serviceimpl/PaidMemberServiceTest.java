package cn.lili.modules.member.serviceimpl;

import cn.lili.modules.member.service.PaidMemberService;

import cn.lili.modules.member.service.MemberBenefitQueryService;
import cn.hutool.json.JSONUtil;
import cn.lili.modules.member.entity.dos.*;
import cn.lili.modules.system.entity.dto.PaidMemberSetting;
import cn.lili.modules.member.mapper.PaidMemberOrderMapper;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.member.event.PaidMemberActivatedEvent;
import org.springframework.context.ApplicationEventPublisher;
import cn.lili.modules.store.service.StoreService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import org.junit.jupiter.api.*;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import java.math.BigDecimal;
import java.util.*;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;
/**
 * 会员费订单幂等、归属、续费与商家范围校验。
 * @author Bulbasaur
 * @since 2026-11-05
 */
class PaidMemberServiceTest {
    /**
     * 被测试的付费会员服务。
     */
    private final PaidMemberService service=new PaidMemberServiceImpl();
    /**
     * 模拟会员购买订单存储。
     */
    private final PaidMemberOrderMapper orders=mock(PaidMemberOrderMapper.class);
    /**
     * 模拟统一配置读写。
     */
    private final SettingService settings=mock(SettingService.class);
    /**
     * 模拟客户身份及行锁查询。
     */
    private final MemberService members=mock(MemberService.class);
    /**
     * 模拟公共权益查询。
     */
    private final MemberBenefitQueryService benefits=mock(MemberBenefitQueryService.class);
    /** 捕获支付成功事件，发放由独立的提交后监听器处理。 */
    private final ApplicationEventPublisher events=mock(ApplicationEventPublisher.class);
    /**
     * 模拟商家归属及自营标记。
     */
    private final StoreService stores=mock(StoreService.class);
    /**
     * 模拟折扣及推荐 SKU 查询。
     */
    private final cn.lili.modules.goods.service.GoodsSkuService skus=mock(cn.lili.modules.goods.service.GoodsSkuService.class);
    /**
     * 各场景使用的单一会员配置样本。
     */
    private final PaidMemberSetting config=new PaidMemberSetting();
    /**
     * 注入模拟依赖及可购买配置，初始化客户和当前线程事务同步环境。
     */
    @BeforeEach void setup(){
        ReflectionTestUtils.setField(service,"orders",orders);ReflectionTestUtils.setField(service,"settings",settings);
        ReflectionTestUtils.setField(service,"members",members);ReflectionTestUtils.setField(service,"benefits",benefits);ReflectionTestUtils.setField(service,"stores",stores);
        ReflectionTestUtils.setField(service,"skus",skus);
        ReflectionTestUtils.setField(service,"events",events);
        var member=new Member();member.setId("member");member.setNickName("测试会员");when(members.getOne(any())).thenReturn(member);
        config.setEnabled(true);config.setAgreement("测试会员协议");var setting=new Setting();setting.setId(SettingEnum.PAID_MEMBER_SETTING.name());setting.setSettingValue(JSONUtil.toJsonStr(config));when(settings.get(SettingEnum.PAID_MEMBER_SETTING.name())).thenReturn(setting);
        when(settings.saveUpdate(any(Setting.class))).thenReturn(true);
        TransactionSynchronizationManager.initSynchronization();
    }
    /**
     * 清除当前线程事务同步状态，防止影响其他测试。
     */
    @AfterEach void cleanup(){TransactionSynchronizationManager.clearSynchronization();}
    /**
     * 验证默认配置关闭，并以 PAID_MEMBER_SETTING 键保存完整配置 JSON。
     */
    @Test void missingSettingUsesDefaultsAndSaveCreatesUnifiedSettingKey() {
        when(settings.get(SettingEnum.PAID_MEMBER_SETTING.name())).thenReturn(null);
        assertThat(service.config().getEnabled()).isFalse();
        service.saveConfig(config);
        var saved=org.mockito.ArgumentCaptor.forClass(Setting.class);
        verify(settings).saveUpdate(saved.capture());
        assertThat(saved.getValue().getId()).isEqualTo("PAID_MEMBER_SETTING");
        var restored=JSONUtil.toBean(saved.getValue().getSettingValue(),PaidMemberSetting.class);
        assertThat(restored.getPrice()).isEqualByComparingTo(config.getPrice());
        assertThat(restored.getUnopenedText()).isEqualTo(config.getUnopenedText());
    }
    /**
     * 验证标签取最新展示配置，八八折仍使用已购订单快照。
     */
    @Test void priceLabelUsesCurrentDisplaySettingsWithoutChangingPaidDiscountSnapshot() {
        var oldConfig=new PaidMemberSetting();oldConfig.setDiscount(new BigDecimal("8.8"));oldConfig.setPriceLabel("old.png");
        var order=unpaid();order.setConfigSnapshot(JSONUtil.toJsonStr(oldConfig));when(orders.selectOne(any())).thenReturn(order);
        config.setPriceLabel("https://example.com/member-label.png");
        var setting=new Setting();setting.setId(SettingEnum.PAID_MEMBER_SETTING.name());setting.setSettingValue(JSONUtil.toJsonStr(config));when(settings.get(SettingEnum.PAID_MEMBER_SETTING.name())).thenReturn(setting);
        var sku=new GoodsSku();sku.setId("sku");sku.setStoreId("self");sku.setSalesModel("RETAIL");sku.setGoodsType("PHYSICAL_GOODS");sku.setMarketEnable("UPPER");sku.setAuthFlag("PASS");sku.setPrice(100D);
        when(skus.listByIds(anyCollection())).thenReturn(List.of(sku));var store=new Store();store.setSelfOperated(true);when(stores.getById("self")).thenReturn(store);
        var quote=(Map<?,?>)service.quotes("member",List.of("sku")).get("sku");
        assertThat(quote.get("price").toString()).isEqualTo("88.00");
        assertThat(quote.get("label")).isEqualTo("https://example.com/member-label.png");
    }
    /**
     * 验证自定义范围拒绝其他商家的折扣 SKU，且不保存非法配置。
     */
    @Test void customStoreScopeRejectsSkuFromOtherStore() {
        config.setStoreScope("CUSTOM");config.setStoreIds(List.of("chosen"));
        var store=new Store();store.setId("chosen");when(stores.listByIds(anyCollection())).thenReturn(List.of(store));
        var rule=new PaidMemberSetting.GoodsRule();rule.setSkuId("sku");rule.setDiscount(new BigDecimal("8.8"));config.setGoodsRules(List.of(rule));
        var sku=new GoodsSku();sku.setId("sku");sku.setStoreId("outside");when(skus.listByIds(anyCollection())).thenReturn(List.of(sku));
        assertThatThrownBy(()->service.saveConfig(config)).hasMessageContaining("折扣商品必须属于");
        verify(settings,never()).saveUpdate(any(Setting.class));
    }
    /**
     * 验证自营范围拒绝第三方商家的折扣商品。
     */
    @Test void selfStoreScopeRejectsThirdPartySku() {
        var rule=new PaidMemberSetting.GoodsRule();rule.setSkuId("sku");rule.setDiscount(new BigDecimal("8.8"));config.setGoodsRules(List.of(rule));
        var sku=new GoodsSku();sku.setId("sku");sku.setStoreId("third-party");when(skus.listByIds(anyCollection())).thenReturn(List.of(sku));
        var store=new Store();store.setId("third-party");store.setSelfOperated(false);when(stores.listByIds(anyCollection())).thenReturn(List.of(store));
        assertThatThrownBy(()->service.saveConfig(config)).hasMessageContaining("折扣商品必须属于");
    }
    /**
     * 验证折扣商品属于参与商家即可保存，推荐商品不受折扣商家范围限制。
     */
    @Test void customScopeAcceptsParticipatingSkuWithoutRestrictingRecommendationStores() {
        config.setStoreScope("CUSTOM");config.setStoreIds(List.of("chosen"));config.setRecommendedSkuIds(List.of("recommended"));
        var store=new Store();store.setId("chosen");when(stores.listByIds(anyCollection())).thenReturn(List.of(store));
        var rule=new PaidMemberSetting.GoodsRule();rule.setSkuId("sku");rule.setDiscount(new BigDecimal("8.8"));config.setGoodsRules(List.of(rule));
        var sku=new GoodsSku();sku.setId("sku");sku.setStoreId("chosen");
        var recommended=new GoodsSku();recommended.setId("recommended");recommended.setStoreId("other");
        when(skus.listByIds(anyCollection())).thenReturn(List.of(sku,recommended));
        service.saveConfig(config);
        verify(settings).saveUpdate(any(Setting.class));
    }
    /**
     * 构造 99 元、12 个月及当前配置快照的未支付订单。
     *
     * @return 购买与回调场景共用的订单样本
     */
    private PaidMemberOrder unpaid(){var order=new PaidMemberOrder();order.setId("order");order.setSn("PM-order");order.setMemberId("member");order.setAmount(new BigDecimal("99"));order.setMonths(12);order.setStatus("UNPAID");order.setConfigSnapshot(JSONUtil.toJsonStr(config));return order;}
    /**
     * 验证购买须同意协议且配置版本一致，合法购买保存未支付状态及快照。
     */
    @Test void purchaseRequiresAgreementAndMatchingCurrentConfiguration(){
        assertThatThrownBy(()->service.create("member","request_123","old",false)).isInstanceOf(RuntimeException.class);
        assertThatThrownBy(()->service.create("member","request_123","old",true)).isInstanceOf(RuntimeException.class);
        verify(orders,never()).insert(any(PaidMemberOrder.class));
        try (var ids=mockStatic(cn.lili.common.utils.SnowFlake.class)) {
            ids.when(cn.lili.common.utils.SnowFlake::getIdStr).thenReturn("test-order-id");
            var order=service.create("member","request_123",service.version(service.config()),true);
            assertThat(order.getAmount()).isEqualByComparingTo("99");assertThat(order.getStatus()).isEqualTo("UNPAID");assertThat(order.getConfigSnapshot()).contains("benefitIds");
        }
    }
    /**
     * 验证重复请求返回已有订单，不因配置版本变化重复创建。
     */
    @Test void samePurchaseRequestNeverCreatesAnotherOrder(){
        var prior=unpaid();when(orders.selectOne(any())).thenReturn(prior);
        assertThat(service.create("member","request_123","old",true)).isSameAs(prior);verify(orders,never()).insert(any(PaidMemberOrder.class));
    }
    /**
     * 验证关闭购买阻止新订单，但不撤销已支付且有效的会员资格。
     */
    @Test void disabledPurchaseDoesNotChangeAlreadyPaidQualification(){
        var setting=new Setting();setting.setId(SettingEnum.PAID_MEMBER_SETTING.name());config.setEnabled(false);setting.setSettingValue(JSONUtil.toJsonStr(config));when(settings.get(SettingEnum.PAID_MEMBER_SETTING.name())).thenReturn(setting);
        assertThatThrownBy(()->service.create("member","request_123",service.version(config),true)).isInstanceOf(RuntimeException.class);
        var active=unpaid();active.setStatus("PAID");active.setStartTime(new Date(System.currentTimeMillis()-10000));active.setEndTime(new Date(System.currentTimeMillis()+10000));assertThat(service.isActive(active)).isTrue();
    }
    /**
     * 验证续费从旧有效期结束时开始且未来区间未生效；重复通知不再延期或发布发放事件。
     */
    @Test void renewalStartsAtPreviousEndAndDuplicateCallbackCannotExtendAgain(){
        var order=unpaid();var prior=unpaid();prior.setEndTime(new Date(System.currentTimeMillis()+86400000));
        when(orders.selectOne(any())).thenReturn(order,prior,order);
        service.paid(order.getSn(),99D,"WECHAT","transaction");Date end=order.getEndTime();
        assertThat(order.getStartTime()).isEqualTo(prior.getEndTime());assertThat(service.isActive(order)).isFalse();
        service.paid(order.getSn(),99D,"WECHAT","transaction");assertThat(order.getEndTime()).isEqualTo(end);verify(orders,times(1)).updateById(order);
        var event=org.mockito.ArgumentCaptor.forClass(PaidMemberActivatedEvent.class);
        verify(events,times(1)).publishEvent(event.capture());
        assertThat(event.getValue().getOrderId()).isEqualTo(order.getId());
    }
    /**
     * 验证错误金额及不支持的支付渠道不能激活会员。
     */
    @Test void wrongAmountAndUnsupportedChannelCannotActivate(){
        var order=unpaid();when(orders.selectOne(any())).thenReturn(order);
        assertThatThrownBy(()->service.paid(order.getSn(),98D,"WECHAT","transaction")).isInstanceOf(RuntimeException.class);
        assertThatThrownBy(()->service.paid(order.getSn(),99D,"NOT_ACTUALLY_PAID","transaction")).isInstanceOf(RuntimeException.class);
        assertThat(order.getStatus()).isEqualTo("UNPAID");verify(orders,never()).updateById(any(PaidMemberOrder.class));
    }
    /**
     * 验证他人不能访问订单，已结束区间不具备会员资格。
     */
    @Test void otherMembersCannotAccessOrdersAndExpiredIntervalIsNotActive(){
        var order=unpaid();when(orders.selectOne(any())).thenReturn(order);
        assertThatThrownBy(()->service.ownedOrder("other",order.getSn())).isInstanceOf(RuntimeException.class);
        order.setStatus("PAID");order.setStartTime(new Date(0));order.setEndTime(new Date(1));assertThat(service.isActive(order)).isFalse();
    }
    /**
     * 验证自营范围排除第三方商品，自营实物商品适用折扣，虚拟商品不适用。
     */
    @Test void merchantAndSkuScopeExcludeVirtualGoods(){
        var sku=new GoodsSku();sku.setId("sku");sku.setStoreId("store");sku.setSalesModel("RETAIL");sku.setGoodsType("PHYSICAL_GOODS");
        var store=new Store();store.setSelfOperated(false);when(stores.getById("store")).thenReturn(store);
        assertThat(service.discount(config,sku)).isNull();store.setSelfOperated(true);assertThat(service.discount(config,sku)).isEqualByComparingTo("9.5");
        sku.setGoodsType("VIRTUAL_GOODS");assertThat(service.discount(config,sku)).isNull();
    }

    /** 会员到期后累计优惠仍按客户汇总，不依赖当前有效会员费订单。 */
    @Test void expiredMembershipKeepsCustomerLifetimeSavings() {
        var latest=new PaidMemberOrder(); latest.setId("old-membership"); latest.setEndTime(new Date(0));
        when(orders.selectOne(any())).thenReturn(null,latest,null);
        when(orders.savedByMember("member")).thenReturn(new BigDecimal("42.35"));
        var current=service.current("member");
        assertThat(current.get("status")).isEqualTo("EXPIRED");
        assertThat(current.get("savedAmount")).isEqualTo(new BigDecimal("42.35"));
        verify(orders).savedByMember("member");
        verify(orders,never()).savedByMember("old-membership");
    }

    /** 购买记录不再查询每笔会员订单的优惠金额，避免多次续费重复展示客户累计值。 */
    @Test void purchaseRecordsDoNotAttachLifetimeSavingsToEachOrder() {
        var page=new com.baomidou.mybatisplus.extension.plugins.pagination.Page<PaidMemberOrder>();
        page.setRecords(List.of(new PaidMemberOrder(),new PaidMemberOrder()));
        when(orders.selectPage(any(),any())).thenReturn(page);
        assertThat(service.page("member",null,null,new cn.lili.common.vo.PageVO()).getRecords()).hasSize(2);
        verify(orders,never()).savedByMember(anyString());
    }
}
