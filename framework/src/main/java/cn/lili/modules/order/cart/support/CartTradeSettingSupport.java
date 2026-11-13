package cn.lili.modules.order.cart.support;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.member.entity.dos.MemberAddress;
import cn.lili.modules.member.service.MemberAddressService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.entity.vo.ReceiptVO;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.dos.StoreAddress;
import cn.lili.modules.store.service.StoreAddressService;
import cn.lili.modules.store.service.StoreService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;

/**
 * 购物车结算配置与前置校验支持类，负责收货地址、自提点、发票、配送方式、礼品卡选择
 * 以及下单前收货地址合法性校验与可用配送方式列表构建。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class CartTradeSettingSupport {

    @Autowired
    private MemberAddressService memberAddressService;

    @Autowired
    private StoreAddressService storeAddressService;

    @Autowired
    private StoreService storeService;

    @Autowired
    private CardKeyService cardKeyService;

    @Autowired
    private AppointmentService appointmentService;

    public CartTradeSettingSupport() {
    }

    public CartTradeSettingSupport(MemberAddressService memberAddressService,
                                   StoreAddressService storeAddressService,
                                   StoreService storeService,
                                   CardKeyService cardKeyService,
                                   AppointmentService appointmentService) {
        this.memberAddressService = memberAddressService;
        this.storeAddressService = storeAddressService;
        this.storeService = storeService;
        this.cardKeyService = cardKeyService;
        this.appointmentService = appointmentService;
    }

    /**
     * 为交易上下文设置会员收货地址。
     *
     * @param tradeDTO          交易上下文
     * @param shippingAddressId 收货地址ID
     */
    public void applyShippingAddress(TradeDTO tradeDTO, String shippingAddressId) {
        MemberAddress memberAddress = memberAddressService.getById(shippingAddressId);
        tradeDTO.setMemberAddress(memberAddress);
    }

    /**
     * 为交易上下文设置商家自提点地址。
     *
     * @param tradeDTO      交易上下文
     * @param shopAddressId 自提点地址ID
     */
    public void applySelfAddress(TradeDTO tradeDTO, String shopAddressId) {
        StoreAddress storeAddress = storeAddressService.getById(shopAddressId);
        tradeDTO.setStoreAddress(storeAddress);
    }

    /**
     * 为交易上下文设置发票开具信息。
     *
     * @param tradeDTO  交易上下文
     * @param receiptVO 发票信息
     */
    public void applyReceipt(TradeDTO tradeDTO, ReceiptVO receiptVO) {
        tradeDTO.setNeedReceipt(true);
        tradeDTO.setReceiptVO(receiptVO);
    }

    /**
     * 为交易上下文中的所有购物车SKU设置统一配送方式。
     *
     * @param tradeDTO       交易上下文
     * @param deliveryMethod 配送方式枚举名称
     */
    public void applyShippingMethod(TradeDTO tradeDTO, String deliveryMethod) {
        String method = DeliveryMethodEnum.valueOf(deliveryMethod).name();
        for (CartSkuVO cartSkuVO : tradeDTO.getSkuList()) {
            cartSkuVO.setDeliveryMethod(method);
        }
    }

    /**
     * 选中或取消选择礼品卡凭证ID并维护去重列表。
     *
     * @param tradeDTO     交易上下文
     * @param credentialId 礼品卡凭证ID
     * @param use          true-选中，false-取消（credentialId为空时清空全部）
     */
    public void applyGiftCardSelection(TradeDTO tradeDTO, String credentialId, boolean use) {
        List<String> selectedIds = new ArrayList<>();
        if (CollUtil.isNotEmpty(tradeDTO.getSelectedGiftCardIds())) {
            selectedIds.addAll(tradeDTO.getSelectedGiftCardIds());
        }
        if (use) {
            if (CharSequenceUtil.isBlank(credentialId)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡ID不能为空");
            }
            if (!selectedIds.contains(credentialId)) {
                selectedIds.add(credentialId);
            }
        } else {
            if (CharSequenceUtil.isBlank(credentialId)) {
                selectedIds.clear();
            } else {
                selectedIds.removeIf(id -> Objects.equals(id, credentialId));
            }
        }
        if (CollUtil.isEmpty(selectedIds)) {
            tradeDTO.setSelectedGiftCardIds(new ArrayList<>());
            return;
        }
        tradeDTO.setSelectedGiftCardIds(new ArrayList<>(new LinkedHashSet<>(selectedIds)));
    }

    /**
     * 根据交易内店铺与商品类型构建结算页可选的配送方式列表。
     *
     * @param tradeDTO 已选中商品渲染后的交易上下文
     * @return 可用配送方式名称列表
     */
    public List<String> buildShippingMethodList(TradeDTO tradeDTO) {
        if (cardKeyService.isECouponOnlyTrade(tradeDTO) || appointmentService.isAppointmentOnlyTrade(tradeDTO)) {
            return List.of(DeliveryMethodEnum.VIRTUAL.name());
        }
        List<String> list = new ArrayList<>();
        list.add(DeliveryMethodEnum.LOGISTICS.name());
        if (tradeDTO.getCartList().size() == 1) {
            for (CartVO cartVO : tradeDTO.getCartList()) {
                Store store = storeService.getById(cartVO.getStoreId());
                if (store.getSelfPickFlag() != null && store.getSelfPickFlag()) {
                    list.add(DeliveryMethodEnum.SELF_PICK_UP.name());
                }
            }
        }
        return list;
    }

    /**
     * 提交交易前校验收货地址（E_COUPON、APPOINTMENT及核销型虚拟商品免收货地址）。
     *
     * @param tradeDTO 待创建交易上下文
     */
    public void validateAddressBeforeTrade(TradeDTO tradeDTO) {
        if (tradeDTO.getStoreAddress() == null && tradeDTO.getMemberAddress() == null
                && !cardKeyService.isECouponOnlyTrade(tradeDTO)
                && !appointmentService.isAppointmentOnlyTrade(tradeDTO)) {
            String goodsType = tradeDTO.getCheckedSkuList().get(0).getGoodsSku().getGoodsType();
            if (!GoodsTypeEnum.VIRTUAL_GOODS.name().equals(goodsType)) {
                throw new ServiceException(ResultCode.MEMBER_ADDRESS_NOT_EXIST);
            }
        }
    }
}
