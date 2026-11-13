package cn.lili.modules.order.cart.support;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.member.entity.dos.MemberAddress;
import cn.lili.modules.member.service.MemberAddressService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.entity.vo.ReceiptVO;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.dos.StoreAddress;
import cn.lili.modules.store.service.StoreAddressService;
import cn.lili.modules.store.service.StoreService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 购物车结算配置与前置校验支持类单元测试，验证地址、发票、配送方式、礼品卡选择及下单前置校验逻辑。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class CartTradeSettingSupportTest {

    private MemberAddressService memberAddressService;
    private StoreAddressService storeAddressService;
    private StoreService storeService;
    private CardKeyService cardKeyService;
    private AppointmentService appointmentService;
    private CartTradeSettingSupport settingSupport;

    @BeforeEach
    void setUp() {
        memberAddressService = mock(MemberAddressService.class);
        storeAddressService = mock(StoreAddressService.class);
        storeService = mock(StoreService.class);
        cardKeyService = mock(CardKeyService.class);
        appointmentService = mock(AppointmentService.class);
        settingSupport = new CartTradeSettingSupport(
                memberAddressService, storeAddressService, storeService, cardKeyService, appointmentService);
    }

    /**
     * 验证收货地址、自提点、发票及配送方式配置正确写入交易上下文。
     */
    @Test
    @DisplayName("收货地址、自提点、发票与配送方式配置正确写入")
    void shouldApplyTradeSettings() {
        TradeDTO tradeDTO = new TradeDTO(CartTypeEnum.CART);
        GoodsSku sku = new GoodsSku();
        sku.setId("SKU-1");
        tradeDTO.getSkuList().add(new CartSkuVO(sku, Collections.emptyMap()));

        MemberAddress addr = new MemberAddress();
        addr.setId("ADDR-1");
        when(memberAddressService.getById("ADDR-1")).thenReturn(addr);

        StoreAddress storeAddr = new StoreAddress();
        storeAddr.setId("SADDR-1");
        when(storeAddressService.getById("SADDR-1")).thenReturn(storeAddr);

        ReceiptVO receiptVO = new ReceiptVO();
        receiptVO.setReceiptTitle("个人");

        settingSupport.applyShippingAddress(tradeDTO, "ADDR-1");
        settingSupport.applySelfAddress(tradeDTO, "SADDR-1");
        settingSupport.applyReceipt(tradeDTO, receiptVO);
        settingSupport.applyShippingMethod(tradeDTO, DeliveryMethodEnum.SELF_PICK_UP.name());

        assertThat(tradeDTO.getMemberAddress()).isSameAs(addr);
        assertThat(tradeDTO.getStoreAddress()).isSameAs(storeAddr);
        assertThat(tradeDTO.getNeedReceipt()).isTrue();
        assertThat(tradeDTO.getReceiptVO()).isSameAs(receiptVO);
        assertThat(tradeDTO.getSkuList().get(0).getDeliveryMethod()).isEqualTo(DeliveryMethodEnum.SELF_PICK_UP.name());
    }

    /**
     * 验证礼品卡选中（去重/空ID拦截）、取消单张以及清空全部。
     */
    @Test
    @DisplayName("礼品卡凭证支持选中去重、单张移除、清空及空ID拦截")
    void shouldManageGiftCardSelection() {
        TradeDTO tradeDTO = new TradeDTO(CartTypeEnum.CART);

        assertThatThrownBy(() -> settingSupport.applyGiftCardSelection(tradeDTO, " ", true))
                .isInstanceOf(ServiceException.class)
                .extracting(e -> ((ServiceException) e).getResultCode())
                .isEqualTo(ResultCode.PARAMS_ERROR);

        settingSupport.applyGiftCardSelection(tradeDTO, "GC-1", true);
        settingSupport.applyGiftCardSelection(tradeDTO, "GC-1", true);
        settingSupport.applyGiftCardSelection(tradeDTO, "GC-2", true);
        assertThat(tradeDTO.getSelectedGiftCardIds()).containsExactly("GC-1", "GC-2");

        settingSupport.applyGiftCardSelection(tradeDTO, "GC-1", false);
        assertThat(tradeDTO.getSelectedGiftCardIds()).containsExactly("GC-2");

        settingSupport.applyGiftCardSelection(tradeDTO, null, false);
        assertThat(tradeDTO.getSelectedGiftCardIds()).isEmpty();
    }

    /**
     * 验证配送方式列表：电子卡券/预约仅返回VIRTUAL；开启自提的单店铺返回LOGISTICS与SELF_PICK_UP。
     */
    @Test
    @DisplayName("根据交易商品类型和店铺自提标识返回可选配送方式")
    void shouldBuildShippingMethodList() {
        TradeDTO eCouponTrade = new TradeDTO(CartTypeEnum.BUY_NOW);
        when(cardKeyService.isECouponOnlyTrade(eCouponTrade)).thenReturn(true);
        assertThat(settingSupport.buildShippingMethodList(eCouponTrade))
                .containsExactly(DeliveryMethodEnum.VIRTUAL.name());

        TradeDTO physicalTrade = new TradeDTO(CartTypeEnum.CART);
        CartVO cartVO = new CartVO();
        cartVO.setStoreId("STORE-1");
        physicalTrade.setCartList(List.of(cartVO));
        Store store = new Store();
        store.setId("STORE-1");
        store.setSelfPickFlag(true);
        when(storeService.getById("STORE-1")).thenReturn(store);

        assertThat(settingSupport.buildShippingMethodList(physicalTrade))
                .containsExactly(DeliveryMethodEnum.LOGISTICS.name(), DeliveryMethodEnum.SELF_PICK_UP.name());
    }

    /**
     * 验证提交交易前收货地址校验：实物商品无地址报错，虚拟商品或电子卡券放行。
     */
    @Test
    @DisplayName("提交交易前实物商品缺地址拦截，虚拟商品免地址放行")
    void shouldValidateAddressBeforeTrade() {
        TradeDTO physicalTrade = new TradeDTO(CartTypeEnum.BUY_NOW);
        GoodsSku physicalSku = new GoodsSku();
        physicalSku.setGoodsType(GoodsTypeEnum.PHYSICAL_GOODS.name());
        CartSkuVO physicalCartSku = new CartSkuVO(physicalSku, Collections.emptyMap());
        physicalCartSku.setChecked(true);
        physicalTrade.getSkuList().add(physicalCartSku);

        assertThatThrownBy(() -> settingSupport.validateAddressBeforeTrade(physicalTrade))
                .isInstanceOf(ServiceException.class)
                .extracting(e -> ((ServiceException) e).getResultCode())
                .isEqualTo(ResultCode.MEMBER_ADDRESS_NOT_EXIST);

        TradeDTO virtualTrade = new TradeDTO(CartTypeEnum.BUY_NOW);
        GoodsSku virtualSku = new GoodsSku();
        virtualSku.setGoodsType(GoodsTypeEnum.VIRTUAL_GOODS.name());
        CartSkuVO virtualCartSku = new CartSkuVO(virtualSku, Collections.emptyMap());
        virtualCartSku.setChecked(true);
        virtualTrade.getSkuList().add(virtualCartSku);

        assertThatCode(() -> settingSupport.validateAddressBeforeTrade(virtualTrade))
                .doesNotThrowAnyException();
    }
}
