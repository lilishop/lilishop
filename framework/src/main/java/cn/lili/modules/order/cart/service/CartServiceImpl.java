package cn.lili.modules.order.cart.service;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.TradeParams;
import cn.lili.modules.order.cart.render.TradeBuilder;
import cn.lili.modules.order.cart.support.CartCouponSupport;
import cn.lili.modules.order.cart.support.CartPromotionCheckSupport;
import cn.lili.modules.order.cart.support.CartTradeSettingSupport;
import cn.lili.modules.order.cart.support.CartTradeCacheSupport;
import cn.lili.modules.order.order.entity.dos.Trade;
import cn.lili.modules.order.order.entity.vo.ReceiptVO;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 购物车业务层实现
 * 交易上下文缓存由公共组件维护，单向调用交易构造器，不依赖延迟获取。
 *
 * @author Chopper
 * @since 2020-03-23 12:29 下午
 */
@Slf4j
@Service
public class CartServiceImpl implements CartService {

    static String errorMessage = "购物车异常，请稍后重试";

    @Autowired
    private CartTradeCacheSupport tradeCache;
    @Autowired
    private TradeBuilder tradeBuilder;
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    @Autowired
    private CardKeyService cardKeyService;
    @Autowired
    private AppointmentService appointmentService;
    @Autowired(required = false)
    private CartCouponSupport cartCouponSupport;
    @Autowired(required = false)
    private CartPromotionCheckSupport cartPromotionCheckSupport;
    @Autowired(required = false)
    private CartTradeSettingSupport cartTradeSettingSupport;

    @Override
    public void add(String skuId, Integer num, String cartType, Boolean cover) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        if (num <= 0) {
            throw new ServiceException(ResultCode.CART_NUM_ERROR);
        }
        CartTypeEnum cartTypeEnum = getCartType(cartType);
        GoodsSku dataSku = getCartPromotionCheckSupport().checkGoods(skuId);
        // E_COUPON：禁止 CART 加购，实时校验卡池库存上限
        cardKeyService.validateAddToCart(skuId, num, cartTypeEnum.name());
        // APPOINTMENT_GOODS：禁止 CART，仅 BUY_NOW
        appointmentService.validateAddToCart(skuId, num, cartTypeEnum.name());
        Map<String, Object> promotionMap = promotionGoodsService.getCurrentGoodsPromotion(dataSku, cartTypeEnum.name());

        try {
            // 购物车方式购买需要保存之前的选择，其他方式购买，则直接抹除掉之前的记录
            TradeDTO tradeDTO;
            if (cartTypeEnum.equals(CartTypeEnum.CART)) {
                // 如果存在，则变更数量不做新增，否则新增一个商品进入集合
                tradeDTO = this.readDTO(cartTypeEnum);
                List<CartSkuVO> cartSkuVOS = tradeDTO.getSkuList();
                CartSkuVO cartSkuVO = cartSkuVOS.stream()
                        .filter(i -> i.getGoodsSku().getId().equals(skuId))
                        .findFirst()
                        .orElse(null);

                // 购物车中已经存在，更新数量
                if (cartSkuVO != null && dataSku.getCreateTime().equals(cartSkuVO.getGoodsSku().getCreateTime())) {
                    if (Boolean.TRUE.equals(cover)) {
                        cartSkuVO.setNum(num);
                        getCartPromotionCheckSupport().checkSetGoodsQuantity(cartSkuVO, skuId, num);
                    } else {
                        int oldNum = cartSkuVO.getNum();
                        int newNum = oldNum + num;
                        getCartPromotionCheckSupport().checkSetGoodsQuantity(cartSkuVO, skuId, newNum);
                    }
                    cartSkuVO.setPromotionMap(promotionMap);
                    cartSkuVO.setSubTotal(CurrencyUtil.mul(cartSkuVO.getPurchasePrice(), cartSkuVO.getNum()));
                } else {
                    // 先清理一下无效商品
                    cartSkuVOS.remove(cartSkuVO);
                    cartSkuVO = new CartSkuVO(dataSku, promotionMap);
                    cartSkuVO.setCartType(cartTypeEnum);
                    getCartPromotionCheckSupport().checkSetGoodsQuantity(cartSkuVO, skuId, num);
                    cartSkuVO.setSubTotal(CurrencyUtil.mul(cartSkuVO.getPurchasePrice(), cartSkuVO.getNum()));
                    cartSkuVOS.add(cartSkuVO);
                }

                // 新加入的商品都是选中的
                cartSkuVO.setChecked(true);
            } else {
                tradeDTO = new TradeDTO(cartTypeEnum);
                tradeDTO.setMemberId(currentUser.getId());
                tradeDTO.setMemberName(currentUser.getUsername());
                List<CartSkuVO> cartSkuVOS = tradeDTO.getSkuList();

                CartSkuVO cartSkuVO = new CartSkuVO(dataSku, promotionMap);
                cartSkuVO.setCartType(cartTypeEnum);
                getCartPromotionCheckSupport().checkCart(cartTypeEnum, cartSkuVO, skuId, num);
                cartSkuVO.setSubTotal(CurrencyUtil.mul(cartSkuVO.getPurchasePrice(), cartSkuVO.getNum()));
                cartSkuVOS.add(cartSkuVO);
            }

            tradeDTO.setCartTypeEnum(cartTypeEnum);
            remoteCoupon(tradeDTO);
            this.resetTradeDTO(tradeDTO);
        } catch (ServiceException serviceException) {
            throw serviceException;
        } catch (Exception e) {
            log.error("购物车渲染异常", e);
            throw new ServiceException(errorMessage);
        }
    }

    /**
     * 复用缓存组件读取当前会员交易上下文，保留默认地址、特殊商品归一化和读取失败处理规则。
     *
     * @param checkedWay 购买方式
     * @return 初始化后的交易上下文
     */
    @Override
    public TradeDTO readDTO(CartTypeEnum checkedWay) {
        return tradeCache.readDTO(checkedWay);
    }

    @Override
    public void checked(String skuId, boolean checked) {
        TradeDTO tradeDTO = this.readDTO(CartTypeEnum.CART);
        remoteCoupon(tradeDTO);
        for (CartSkuVO cartSkuVO : tradeDTO.getSkuList()) {
            if (cartSkuVO.getGoodsSku().getId().equals(skuId)) {
                cartSkuVO.setChecked(checked);
            }
        }
        this.resetTradeDTO(tradeDTO);
    }

    @Override
    public void checkedStore(String storeId, boolean checked) {
        TradeDTO tradeDTO = this.readDTO(CartTypeEnum.CART);
        remoteCoupon(tradeDTO);
        for (CartSkuVO cartSkuVO : tradeDTO.getSkuList()) {
            if (cartSkuVO.getStoreId().equals(storeId)) {
                cartSkuVO.setChecked(checked);
            }
        }
        resetTradeDTO(tradeDTO);
    }

    @Override
    public void checkedAll(boolean checked) {
        TradeDTO tradeDTO = this.readDTO(CartTypeEnum.CART);
        remoteCoupon(tradeDTO);
        for (CartSkuVO cartSkuVO : tradeDTO.getSkuList()) {
            cartSkuVO.setChecked(checked);
        }
        resetTradeDTO(tradeDTO);
    }

    /**
     * 当购物车商品发生变更时，取消已选择的优惠券
     *
     * @param tradeDTO 交易上下文
     */
    private void remoteCoupon(TradeDTO tradeDTO) {
        tradeDTO.setPlatformCoupon(null);
        tradeDTO.setStoreCoupons(new HashMap<>());
    }

    @Override
    public void delete(String[] skuIds) {
        TradeDTO tradeDTO = this.readDTO(CartTypeEnum.CART);
        List<CartSkuVO> cartSkuVOS = tradeDTO.getSkuList();
        List<CartSkuVO> deleteVos = new ArrayList<>();
        for (CartSkuVO cartSkuVO : cartSkuVOS) {
            for (String skuId : skuIds) {
                if (cartSkuVO.getGoodsSku().getId().equals(skuId)) {
                    deleteVos.add(cartSkuVO);
                }
            }
        }
        cartSkuVOS.removeAll(deleteVos);
        resetTradeDTO(tradeDTO);
    }

    @Override
    public void clean() {
        tradeCache.cleanCart();
    }

    public void cleanChecked(TradeDTO tradeDTO) {
        List<CartSkuVO> cartSkuVOS = tradeDTO.getSkuList();
        List<CartSkuVO> deleteVos = new ArrayList<>();
        for (CartSkuVO cartSkuVO : cartSkuVOS) {
            if (Boolean.TRUE.equals(cartSkuVO.getChecked())) {
                deleteVos.add(cartSkuVO);
            }
        }
        cartSkuVOS.removeAll(deleteVos);
        tradeDTO.setPlatformCoupon(null);
        tradeDTO.setStoreCoupons(null);
        tradeDTO.setStoreRemark(null);
        resetTradeDTO(tradeDTO);
    }

    @Override
    public void resetTradeDTO(TradeDTO tradeDTO) {
        tradeCache.resetTradeDTO(tradeDTO);
    }

    @Override
    public TradeDTO getCheckedTradeDTO(CartTypeEnum way) {
        return tradeBuilder.buildChecked(way);
    }

    /**
     * 获取可使用的优惠券数量
     *
     * @param checkedWay 购物车购买：CART/立即购买：BUY_NOW/拼团购买：PINTUAN / 积分购买：POINT
     * @return 可使用的优惠券数量
     */
    @Override
    public Long getCanUseCoupon(CartTypeEnum checkedWay) {
        TradeDTO tradeDTO = this.readDTO(checkedWay);
        return getCartCouponSupport().getCanUseCoupon(tradeDTO);
    }

    @Override
    public TradeDTO getAllTradeDTO() {
        return tradeBuilder.buildCart(CartTypeEnum.CART);
    }

    @Override
    public void shippingAddress(String shippingAddressId, String way) {
        CartTypeEnum cartTypeEnum = resolveCartType(way);
        TradeDTO tradeDTO = this.readDTO(cartTypeEnum);
        getCartTradeSettingSupport().applyShippingAddress(tradeDTO, shippingAddressId);
        this.resetTradeDTO(tradeDTO);
    }

    @Override
    public void shippingSelfAddress(String shopAddressId, String way) {
        CartTypeEnum cartTypeEnum = resolveCartType(way);
        TradeDTO tradeDTO = this.readDTO(cartTypeEnum);
        getCartTradeSettingSupport().applySelfAddress(tradeDTO, shopAddressId);
        this.resetTradeDTO(tradeDTO);
    }

    /**
     * 选择发票
     *
     * @param receiptVO 发票信息
     * @param way       购物车类型
     */
    @Override
    public void shippingReceipt(ReceiptVO receiptVO, String way) {
        CartTypeEnum cartTypeEnum = resolveCartType(way);
        TradeDTO tradeDTO = this.readDTO(cartTypeEnum);
        getCartTradeSettingSupport().applyReceipt(tradeDTO, receiptVO);
        this.resetTradeDTO(tradeDTO);
    }

    /**
     * 选择配送方式
     *
     * @param deliveryMethod 配送方式
     * @param way            购物车类型
     */
    @Override
    public void shippingMethod(String deliveryMethod, String way) {
        CartTypeEnum cartTypeEnum = resolveCartType(way);
        TradeDTO tradeDTO = this.readDTO(cartTypeEnum);
        getCartTradeSettingSupport().applyShippingMethod(tradeDTO, deliveryMethod);
        this.resetTradeDTO(tradeDTO);
    }

    /**
     * 获取购物车商品数量
     *
     * @param checked 是否选择
     * @return 购物车商品数量
     */
    @Override
    public Long getCartNum(Boolean checked) {
        TradeDTO tradeDTO = this.getAllTradeDTO();
        List<CartSkuVO> collect = tradeDTO.getSkuList().stream()
                .filter(i -> Boolean.FALSE.equals(i.getInvalid()))
                .collect(Collectors.toList());
        long count = 0L;
        if (!tradeDTO.getSkuList().isEmpty()) {
            if (checked != null) {
                count = collect.stream().filter(i -> i.getChecked().equals(checked)).count();
            } else {
                count = collect.size();
            }
        }
        return count;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void selectCoupon(String couponId, String way, boolean use) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        CartTypeEnum cartTypeEnum = getCartType(way);
        if (cartTypeEnum.equals(CartTypeEnum.POINTS)) {
            throw new ServiceException(ResultCode.SPECIAL_CANT_USE);
        }
        TradeDTO tradeDTO = this.readDTO(cartTypeEnum);
        getCartCouponSupport().selectCoupon(tradeDTO, currentUser.getId(), couponId, use, cartTypeEnum);
        this.resetTradeDTO(tradeDTO);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void selectGiftCard(String credentialId, String way, boolean use) {
        CartTypeEnum cartTypeEnum = getCartType(way);
        TradeDTO tradeDTO = this.readDTO(cartTypeEnum);
        getCartTradeSettingSupport().applyGiftCardSelection(tradeDTO, credentialId, use);
        this.resetTradeDTO(tradeDTO);
    }

    @Override
    public Trade createTrade(TradeParams tradeParams) {
        CartTypeEnum cartTypeEnum = getCartType(tradeParams.getWay());
        TradeDTO tradeDTO = this.readDTO(cartTypeEnum);
        tradeDTO.setClientType(tradeParams.getClient());
        tradeDTO.setStoreRemark(tradeParams.getRemark());
        tradeDTO.setParentOrderSn(tradeParams.getParentOrderSn());
        getCartTradeSettingSupport().validateAddressBeforeTrade(tradeDTO);
        Trade trade = tradeBuilder.createTrade(tradeDTO);
        this.cleanChecked(this.readDTO(cartTypeEnum));
        return trade;
    }

    @Override
    public List<String> shippingMethodList(String way) {
        TradeDTO tradeDTO = this.getCheckedTradeDTO(CartTypeEnum.valueOf(way));
        return getCartTradeSettingSupport().buildShippingMethodList(tradeDTO);
    }

    private CartTypeEnum resolveCartType(String way) {
        CartTypeEnum cartTypeEnum = CartTypeEnum.CART;
        if (CharSequenceUtil.isNotEmpty(way)) {
            cartTypeEnum = CartTypeEnum.valueOf(way);
        }
        return cartTypeEnum;
    }

    /**
     * 获取购物车类型（非法枚举值回退默认 CART）
     *
     * @param way 购物车类型字符串
     * @return 购物车枚举
     */
    private CartTypeEnum getCartType(String way) {
        CartTypeEnum cartTypeEnum = CartTypeEnum.CART;
        if (CharSequenceUtil.isNotEmpty(way)) {
            try {
                cartTypeEnum = CartTypeEnum.valueOf(way);
            } catch (IllegalArgumentException e) {
                log.error("获取购物车类型出现错误：", e);
            }
        }
        return cartTypeEnum;
    }

    private CartCouponSupport getCartCouponSupport() {
        if (this.cartCouponSupport == null) {
            this.cartCouponSupport = new CartCouponSupport();
        }
        return this.cartCouponSupport;
    }

    private CartPromotionCheckSupport getCartPromotionCheckSupport() {
        if (this.cartPromotionCheckSupport == null) {
            this.cartPromotionCheckSupport = new CartPromotionCheckSupport();
        }
        return this.cartPromotionCheckSupport;
    }

    private CartTradeSettingSupport getCartTradeSettingSupport() {
        if (this.cartTradeSettingSupport == null) {
            this.cartTradeSettingSupport = new CartTradeSettingSupport();
        }
        return this.cartTradeSettingSupport;
    }
}
