package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.MemberAddress;
import cn.lili.modules.member.service.MemberAddressService;
import cn.lili.modules.order.aftersale.serviceimpl.AfterSaleServiceImpl;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.render.impl.CartPriceRender;
import cn.lili.modules.order.cart.render.impl.CheckDataRender;
import cn.lili.modules.order.cart.render.impl.SkuFreightRender;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.Trade;
import cn.lili.modules.order.order.service.TradeService;
import cn.lili.modules.order.order.serviceimpl.OrderServiceImpl;
import cn.lili.modules.promotion.entity.dos.GiftCardCashActivity;
import cn.lili.modules.promotion.entity.dos.GiftCardCashCredential;
import cn.lili.modules.promotion.entity.dto.PickupCardRedeemDTO;
import cn.lili.modules.promotion.mapper.GiftCardCashActivityMapper;
import cn.lili.modules.promotion.mapper.GiftCardCashCredentialMapper;
import cn.lili.modules.promotion.service.GiftCardCashService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Date;
import java.util.List;

import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * 提货卡兑换风险测试：商品归属与数量、越权、过期、幂等、销售价结算和禁止退款。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
class PickupCardServiceImplTest {
    private PickupCardServiceImpl service;
    private PickupCardRuleServiceImpl rules;
    private GiftCardCashCredentialMapper cards;
    private GiftCardCashActivityMapper activities;
    private GoodsSkuService goods;
    private MemberAddressService addresses;
    private TradeService trades;
    private GiftCardCashCredential card;
    private PickupCardRedeemDTO request;

    /**
     * 初始化独立模拟依赖，测试不连接数据库或 Redis。
     */
    @BeforeEach
    void setUp() {
        service = new PickupCardServiceImpl();
        rules = new PickupCardRuleServiceImpl();
        ReflectionTestUtils.setField(service, "pickupCardRuleService", rules);
        cards = mock(GiftCardCashCredentialMapper.class);
        activities = mock(GiftCardCashActivityMapper.class);
        goods = mock(GoodsSkuService.class);
        addresses = mock(MemberAddressService.class);
        trades = mock(TradeService.class);
        ReflectionTestUtils.setField(service, "credentialMapper", cards);
        ReflectionTestUtils.setField(service, "activityMapper", activities);
        ReflectionTestUtils.setField(service, "goodsSkuService", goods);
        ReflectionTestUtils.setField(rules, "goodsSkuService", goods);
        ReflectionTestUtils.setField(service, "memberAddressService", addresses);
        ReflectionTestUtils.setField(service, "tradeService", trades);
        card = new GiftCardCashCredential();
        card.setId("card");
        card.setCardType("PICKUP");
        card.setActivityId("activity");
        card.setMemberId("member");
        card.setActivateMemberId("member");
        card.setActivateTime(new Date());
        card.setStatus("BOUND");
        when(cards.selectOne(any())).thenReturn(card);
        request = new PickupCardRedeemDTO();
        request.setCredentialId("card");
        request.setSkuIds(List.of("sku"));
        request.setAddressId("address");
    }

    /**
     * 多选一、全选及任选 N 均可兑换符合约束的 SKU 集合。
     */
    @Test
    void acceptsAllThreeModes() {
        assertThatCode(() -> rules.validateSelection("ONE", 1, List.of("a", "b"), List.of("b"), true)).doesNotThrowAnyException();
        assertThatCode(() -> rules.validateSelection("ALL", 2, List.of("a", "b"), List.of("b", "a"), true)).doesNotThrowAnyException();
        assertThatCode(() -> rules.validateSelection("ANY", 2, List.of("a", "b", "c"), List.of("c", "a"), true)).doesNotThrowAnyException();
    }

    /**
     * 同一 SKU 重复提交不能伪装成多种商品。
     */
    @Test
    void rejectsDuplicateSku() {
        assertThatThrownBy(() -> rules.validateSelection("ANY", 2, List.of("a", "b"), List.of("a", "a"), true)).isInstanceOf(ServiceException.class);
    }

    /**
     * 兑换商品必须在制卡时锁定的候选集合内。
     */
    @Test
    void rejectsForeignSku() {
        assertThatThrownBy(() -> rules.validateSelection("ONE", 1, List.of("a"), List.of("other"), true)).isInstanceOf(ServiceException.class);
    }

    /**
     * 全部兑换不能少选，避免一次核销丢失部分权益。
     */
    @Test
    void rejectsPartialAllRedemption() {
        assertThatThrownBy(() -> rules.validateSelection("ALL", 2, List.of("a", "b"), List.of("a"), true)).isInstanceOf(ServiceException.class);
    }

    /**
     * 后端拒绝空配置和超出候选数的任选 N。
     */
    @Test
    void rejectsInvalidConfiguration() {
        assertThatThrownBy(() -> rules.validateSelection("ANY", 3, List.of("a", "b"), null, false)).isInstanceOf(ServiceException.class);
        assertThatThrownBy(() -> rules.validateSelection("ONE", 1, List.of(), null, false)).isInstanceOf(ServiceException.class);
    }

    /**
     * 即使已兑换，也必须先校验会员归属，再返回幂等结果。
     */
    @Test
    void rejectsForeignCardBeforeIdempotentReturn() {
        card.setPickupTradeSn("original");
        card.setMemberId("other");
        assertThatThrownBy(() -> service.redeem("member", request)).isInstanceOf(ServiceException.class);
        verifyNoInteractions(trades, goods);
    }

    /**
     * 同一卡重复兑换返回原交易，不再次扣库存、建单或核销。
     */
    @Test
    void returnsOriginalTradeOnRetry() {
        card.setStatus("REDEEMED");
        card.setPickupTradeSn("original");
        card.setExpireTime(new Date(1));
        Trade original = new Trade();
        original.setSn("original");
        when(trades.getBySn("original")).thenReturn(original);
        assertThat(service.redeem("member", request)).isSameAs(original);
        verifyNoInteractions(goods, addresses, activities);
        verify(cards, never()).update(any(), any());
    }

    /**
     * 现金卡不得通过提货入口绕过余额支付。
     */
    @Test
    void rejectsCashCard() {
        card.setCardType("CASH");
        assertThatThrownBy(() -> service.preview("member", request)).isInstanceOf(ServiceException.class);
    }

    /**
     * 已过有效期的卡不能兑换，且不会占库存。
     */
    @Test
    void rejectsExpiredCard() {
        card.setExpireTime(new Date(1));
        assertThatThrownBy(() -> service.preview("member", request)).isInstanceOf(ServiceException.class);
        verifyNoInteractions(goods, addresses);
    }

    /**
     * 价格预览使用当前销售价而非促销价，免邮且商家全额结算。
     */
    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"PC", "H5", "WECHAT_MP", "APP", "UNKNOWN", " "})
    void previewUsesSalesPriceAndFullSettlementAcrossStores(String client) {
        request.setClient(client);
        GiftCardCashActivity activity = new GiftCardCashActivity();
        activity.setPickupMode("ALL");
        activity.setPickupCount(2);
        activity.setPickupSkuIds("[\"sku\",\"sku2\"]");
        request.setSkuIds(List.of("sku", "sku2"));
        when(activities.selectById("activity")).thenReturn(activity);
        MemberAddress address = new MemberAddress();
        address.setMemberId("member");
        when(addresses.getMemberAddress("address")).thenReturn(address);
        GoodsSku sku = new GoodsSku();
        sku.setId("sku");
        sku.setStoreId("store");
        sku.setGoodsType("PHYSICAL_GOODS");
        sku.setMarketEnable("UPPER");
        sku.setAuthFlag("PASS");
        sku.setPrice(128.50);
        sku.setPromotionFlag(true);
        sku.setPromotionPrice(1.0);
        when(goods.getById("sku")).thenReturn(sku);
        when(goods.getStock("sku")).thenReturn(10);
        GoodsSku second = new GoodsSku();
        second.setId("sku2");
        second.setStoreId("store2");
        second.setGoodsType("PHYSICAL_GOODS");
        second.setMarketEnable("UPPER");
        second.setAuthFlag("PASS");
        second.setPrice(39.99);
        when(goods.getById("sku2")).thenReturn(second);
        when(goods.getStock("sku2")).thenReturn(10);
        CheckDataRender check = mock(CheckDataRender.class);
        doAnswer(invocation -> {
            TradeDTO dto = invocation.getArgument(0);
            java.util.List<CartVO> carts = new java.util.ArrayList<>();
            for (var item : dto.getSkuList()) {
                CartVO cart = new CartVO(item);
                cart.setSkuList(List.of(item));
                cart.setChecked(true);
                carts.add(cart);
            }
            dto.setCartList(carts);
            return null;
        }).when(check).render(any());
        ReflectionTestUtils.setField(service, "checkDataRender", check);
        ReflectionTestUtils.setField(service, "freightRender", mock(SkuFreightRender.class));
        ReflectionTestUtils.setField(service, "priceRender", new CartPriceRender());
        try (var context = mockStatic(UserContext.class)) {
            AuthUser user = mock(AuthUser.class);
            when(user.getUsername()).thenReturn("buyer");
            context.when(UserContext::getCurrentUser).thenReturn(user);
            TradeDTO preview = service.preview("member", request);
            String expectedClient = client == null || client.isBlank() ? "UNKNOWN" : client;
            assertThat(preview.getClientType()).isEqualTo(expectedClient);
            assertThat(preview.getCartList()).allSatisfy(cart ->
                    assertThat(new Order(cart, preview).getClientType()).isEqualTo(expectedClient));
            assertThat(preview.getPriceDetailDTO().getGoodsPrice()).isEqualTo(168.49);
            assertThat(preview.getPriceDetailDTO().getFlowPrice()).isZero();
            assertThat(preview.getPriceDetailDTO().getBillPrice()).isEqualTo(168.49);
            assertThat(preview.getPriceDetailDTO().getFreightPrice()).isZero();
            assertThat(preview.getCartList()).hasSize(2);
            assertThat(preview.getCartList()).allSatisfy(cart -> {
                assertThat(cart.getPriceDetailDTO().getFlowPrice()).isZero();
                assertThat(cart.getPriceDetailDTO().getBillPrice()).isEqualTo(cart.getPriceDetailDTO().getGoodsPrice());
            });
            assertThat(preview.getSkuList().getFirst().getPromotionMap()).isEmpty();
            verify(goods, never()).updateGoodsStock(any());
        }
    }

    /** 非法客户端值不能写入订单来源；枚举校验独立于金额及兑换权益。 */
    @Test
    void rejectsInvalidClientType() {
        assertThatThrownBy(() -> ReflectionTestUtils.invokeMethod(service, "resolveClientType", "invalid-client"))
                .isInstanceOf(ServiceException.class);
    }

    /**
     * 取消兑换订单不调用回卡或渠道退款，保留已核销权益。
     */
    @Test
    void cancelNeverRefundsPickupCard() {
        OrderServiceImpl orders = new OrderServiceImpl();
        GiftCardCashService cash = mock(GiftCardCashService.class);
        ReflectionTestUtils.setField(orders, "giftCardCashService", cash);
        Order order = new Order();
        order.setSn("order");
        cn.lili.modules.promotion.service.PickupCardOrderService pickupOrders = mock(cn.lili.modules.promotion.service.PickupCardOrderService.class);
        when(pickupOrders.isPickupOrder("order")).thenReturn(true);
        ReflectionTestUtils.setField(orders, "pickupCardOrderService", pickupOrders);
        order.setPayStatus("PAID");
        ReflectionTestUtils.invokeMethod(orders, "refundCanceledOrderBySplit", order, "buyer cancelled");
        verifyNoInteractions(cash);
    }

    /**
     * 服务端售后入口拒绝提货订单，隐藏前端按钮不能替代后端校验。
     */
    @Test
    void rejectsAfterSale() {
        Order order = new Order();
        order.setSn("order");
        AfterSaleServiceImpl afterSale = new AfterSaleServiceImpl();
        cn.lili.modules.promotion.service.PickupCardOrderService pickupOrders = mock(cn.lili.modules.promotion.service.PickupCardOrderService.class);
        when(pickupOrders.isPickupOrder("order")).thenReturn(true);
        ReflectionTestUtils.setField(afterSale, "pickupCardOrderService", pickupOrders);
        assertThatThrownBy(() -> ReflectionTestUtils.invokeMethod(afterSale, "rejectECouponAfterSale", order)).isInstanceOf(ServiceException.class);
    }

    /**
     * 无现金余额的未使用提货卡仍可用，成功兑换后不可用。
     */
    @Test
    void pickupAvailabilityDoesNotDependOnCashBalance() {
        GiftCardCashServiceImpl cash = new GiftCardCashServiceImpl();
        card.setBalance(java.math.BigDecimal.ZERO);
        assertThat((String) ReflectionTestUtils.invokeMethod(cash, "resolveMemberCardStatus", card, new Date())).isEqualTo("AVAILABLE");
        card.setPickupTradeSn("trade");
        card.setStatus("REDEEMED");
        assertThat((String) ReflectionTestUtils.invokeMethod(cash, "resolveMemberCardStatus", card, new Date())).isEqualTo("UNAVAILABLE");
    }
}
