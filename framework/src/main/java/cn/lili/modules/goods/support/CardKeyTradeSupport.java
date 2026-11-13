package cn.lili.modules.goods.support;

import cn.lili.modules.goods.serviceimpl.CardKeyServiceImpl;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.CardKey;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.CardKeyFulfillStatusEnum;
import cn.lili.modules.goods.entity.enums.CardKeyStatusEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.entity.vo.CardKeyVO;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.goods.util.CardKeySecretUtil;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.vo.GiftECouponOrderSummaryVO;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import cn.lili.modules.store.service.StoreService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;

import java.util.ArrayList;
import java.util.List;
import java.util.function.IntSupplier;

/**
 * 电子卡券订单详情卡密字段增强、满赠概要汇总与交易防刷校验支持类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
public class CardKeyTradeSupport {

    public static final int MAX_BUY_QUANTITY = 999;
    public static final String GIFT_FULFILL_FAIL_CANCEL_REASON = "满赠电子卡券发放失败";

    /**
     * 增强订单详情（填充卡密履约状态、已交付卡密列表及满赠子单摘要）。
     *
     * @param service 卡密服务实现
     * @param orderDetailVO 订单详情展示对象
     * @param orderMapper 订单映射
     * @param orderItemService 订单项服务
     * @param cardKeySecretUtil 卡密解密组件
     */
    public static void enrichOrderDetail(CardKeyServiceImpl service,
                                         OrderDetailVO orderDetailVO,
                                         OrderMapper orderMapper,
                                         OrderItemService orderItemService,
                                         CardKeySecretUtil cardKeySecretUtil) {
        if (orderDetailVO == null || orderDetailVO.getOrder() == null) {
            return;
        }
        Order order = orderDetailVO.getOrder();
        if (OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())
                && CollUtil.isNotEmpty(orderDetailVO.getOrderItems())) {
            for (OrderItem orderItem : orderDetailVO.getOrderItems()) {
                applyCardKeyFulfillToOrderItem(service, order, orderItem, cardKeySecretUtil);
            }
        }
        if (!isGiftECouponOrder(order) && CharSequenceUtil.isBlank(order.getParentOrderSn())) {
            orderDetailVO.setGiftECouponOrders(loadGiftECouponSummaries(service, orderMapper, orderItemService, cardKeySecretUtil, order.getSn()));
        }
    }

    /**
     * 将卡密履约结果注入单个订单项。
     */
    public static void applyCardKeyFulfillToOrderItem(CardKeyServiceImpl service,
                                                     Order order,
                                                     OrderItem orderItem,
                                                     CardKeySecretUtil cardKeySecretUtil) {
        CardKeyFulfillStatusEnum status = resolveCardKeyFulfillStatus(order);
        String message = resolveCardKeyFulfillMessage(order, status);
        List<CardKeyVO> cardKeys = CardKeyFulfillStatusEnum.DELIVERED.equals(status)
                ? listCardKeyVOByOrderItemSn(service, cardKeySecretUtil, orderItem.getSn(), true)
                : new ArrayList<>();
        if (CardKeyFulfillStatusEnum.DELIVERED.equals(status) && cardKeys.isEmpty()) {
            status = CardKeyFulfillStatusEnum.FAILED;
            message = CharSequenceUtil.blankToDefault(order.getCancelReason(), "订单已完成但未找到已分配卡密");
        }
        orderItem.setCardKeyFulfillStatus(status.name());
        orderItem.setCardKeyFulfillMessage(message);
        orderItem.setCardKeys(cardKeys);
        orderItem.setCardKeyDelivered(CardKeyFulfillStatusEnum.DELIVERED.equals(status) && !cardKeys.isEmpty());
    }

    /**
     * 解析订单当前卡密履约状态。
     */
    public static CardKeyFulfillStatusEnum resolveCardKeyFulfillStatus(Order order) {
        if (OrderStatusEnum.CANCELLED.name().equals(order.getOrderStatus())) {
            return CardKeyFulfillStatusEnum.FAILED;
        }
        if (OrderStatusEnum.COMPLETED.name().equals(order.getOrderStatus())
                && PayStatusEnum.PAID.name().equals(order.getPayStatus())) {
            return CardKeyFulfillStatusEnum.DELIVERED;
        }
        if (isPintuanECouponOrder(order)) {
            return CardKeyFulfillStatusEnum.PENDING;
        }
        if (PayStatusEnum.PAID.name().equals(order.getPayStatus())
                || OrderStatusEnum.PAID.name().equals(order.getOrderStatus())) {
            return CardKeyFulfillStatusEnum.PENDING;
        }
        return CardKeyFulfillStatusEnum.PENDING;
    }

    /**
     * 解析订单当前卡密履约文案。
     */
    public static String resolveCardKeyFulfillMessage(Order order, CardKeyFulfillStatusEnum status) {
        if (CardKeyFulfillStatusEnum.FAILED.equals(status)) {
            if (GIFT_FULFILL_FAIL_CANCEL_REASON.equals(order.getCancelReason())) {
                return isGiftECouponOrder(order)
                        ? "满赠电子卡券发放失败，赠品子单已取消，主订单不受影响"
                        : order.getCancelReason();
            }
            return CharSequenceUtil.blankToDefault(order.getCancelReason(), "卡密发放失败");
        }
        if (CardKeyFulfillStatusEnum.DELIVERED.equals(status)) {
            return null;
        }
        if (isPintuanECouponOrder(order)) {
            return "拼团成功后发放卡密";
        }
        if (isGiftECouponOrder(order)) {
            return "满赠卡密发放中，请稍后刷新";
        }
        return "卡密发放中，请稍后刷新";
    }

    /**
     * 加载主订单关联的满赠电子卡券子单概要列表。
     */
    public static List<GiftECouponOrderSummaryVO> loadGiftECouponSummaries(CardKeyServiceImpl service,
                                                                          OrderMapper orderMapper,
                                                                          OrderItemService orderItemService,
                                                                          CardKeySecretUtil cardKeySecretUtil,
                                                                          String parentOrderSn) {
        List<Order> giftOrders = orderMapper.selectList(new LambdaQueryWrapper<Order>()
                .eq(Order::getParentOrderSn, parentOrderSn)
                .eq(Order::getOrderPromotionType, OrderPromotionTypeEnum.GIFT.name())
                .eq(Order::getOrderType, OrderTypeEnum.E_COUPON.name()));
        if (CollUtil.isEmpty(giftOrders)) {
            return new ArrayList<>();
        }
        List<GiftECouponOrderSummaryVO> summaries = new ArrayList<>();
        for (Order giftOrder : giftOrders) {
            GiftECouponOrderSummaryVO summary = new GiftECouponOrderSummaryVO();
            summary.setOrderSn(giftOrder.getSn());
            summary.setParentOrderSn(giftOrder.getParentOrderSn());
            summary.setOrderStatus(giftOrder.getOrderStatus());
            CardKeyFulfillStatusEnum status = resolveCardKeyFulfillStatus(giftOrder);
            summary.setCardKeyFulfillStatus(status.name());
            summary.setCardKeyFulfillMessage(resolveCardKeyFulfillMessage(giftOrder, status));
            List<OrderItem> giftItems = orderItemService.getByOrderSn(giftOrder.getSn());
            if (CollUtil.isNotEmpty(giftItems)) {
                OrderItem first = giftItems.get(0);
                summary.setGoodsName(first.getGoodsName());
                summary.setSkuId(first.getSkuId());
            }
            List<CardKeyVO> cardKeys = new ArrayList<>();
            if (CardKeyFulfillStatusEnum.DELIVERED.equals(status)) {
                for (OrderItem giftItem : giftItems) {
                    cardKeys.addAll(listCardKeyVOByOrderItemSn(service, cardKeySecretUtil, giftItem.getSn(), true));
                }
                if (cardKeys.isEmpty()) {
                    summary.setCardKeyFulfillStatus(CardKeyFulfillStatusEnum.FAILED.name());
                    summary.setCardKeyFulfillMessage(
                            CharSequenceUtil.blankToDefault(giftOrder.getCancelReason(), "订单已完成但未找到已分配卡密"));
                }
            }
            summary.setCardKeys(cardKeys);
            summary.setCardKeyDelivered(!cardKeys.isEmpty());
            summaries.add(summary);
        }
        return summaries;
    }

    /**
     * 按订单项编号查询已分配卡密列表（支持解密）。
     */
    public static List<CardKeyVO> listCardKeyVOByOrderItemSn(CardKeyServiceImpl service,
                                                             CardKeySecretUtil cardKeySecretUtil,
                                                             String orderItemSn,
                                                             boolean includeStatus) {
        List<CardKey> list = service.list(new LambdaQueryWrapper<CardKey>()
                .eq(CardKey::getOrderItemSn, orderItemSn)
                .eq(CardKey::getStatus, CardKeyStatusEnum.ALLOCATED.name())
                .eq(CardKey::getDeleteFlag, false)
                .orderByAsc(CardKey::getAllocatedTime));
        List<CardKeyVO> result = new ArrayList<>();
        for (CardKey cardKey : list) {
            CardKeyVO vo = new CardKeyVO();
            vo.setCardNo(cardKey.getCardNo());
            vo.setCardSecret(cardKeySecretUtil.decrypt(cardKey.getCardSecret()));
            vo.setAllocatedTime(cardKey.getAllocatedTime());
            if (includeStatus) {
                vo.setStatus(cardKey.getStatus());
            }
            result.add(vo);
        }
        return result;
    }

    /**
     * 判断交易中是否全为电子卡券商品。
     */
    public static boolean isECouponOnlyTrade(TradeDTO tradeDTO) {
        if (tradeDTO == null) {
            return false;
        }
        List<CartSkuVO> lines = tradeDTO.getCheckedSkuList();
        if (CollUtil.isEmpty(lines)) {
            lines = tradeDTO.getSkuList();
        }
        if (CollUtil.isEmpty(lines)) {
            return false;
        }
        return lines.stream().allMatch(line -> line.getGoodsSku() != null
                && GoodsTypeEnum.E_COUPON.name().equals(line.getGoodsSku().getGoodsType()));
    }

    /**
     * 规范化纯电子卡券交易参数（清空地址、设置虚拟履约配送方式、清零运费）。
     */
    public static void normalizeECouponTrade(TradeDTO tradeDTO) {
        if (!isECouponOnlyTrade(tradeDTO)) {
            return;
        }
        tradeDTO.setMemberAddress(null);
        tradeDTO.setStoreAddress(null);
        if (tradeDTO.getCartList() != null) {
            for (CartVO cart : tradeDTO.getCartList()) {
                cart.setDeliveryMethod(DeliveryMethodEnum.VIRTUAL.name());
                if (cart.getSkuList() != null) {
                    for (CartSkuVO sku : cart.getSkuList()) {
                        sku.setDeliveryMethod(DeliveryMethodEnum.VIRTUAL.name());
                    }
                }
            }
        }
        if (tradeDTO.getSkuList() != null) {
            for (CartSkuVO sku : tradeDTO.getSkuList()) {
                sku.setDeliveryMethod(DeliveryMethodEnum.VIRTUAL.name());
            }
        }
        PriceDetailDTO priceDetail = tradeDTO.getPriceDetailDTO();
        if (priceDetail != null) {
            priceDetail.setFreightPrice(0D);
        }
    }

    /**
     * 校验电子卡券下单参数（禁止混买、禁止购物车购买、禁止多 SKU、检查库存与店铺开店状态）。
     */
    public static void validateTradeForECoupon(TradeDTO tradeDTO, StoreService storeService, IntSupplier stockSupplier) {
        List<CartSkuVO> checked = tradeDTO.getCheckedSkuList();
        if (CollUtil.isEmpty(checked)) {
            return;
        }
        long eCouponCount = checked.stream()
                .filter(s -> GoodsTypeEnum.E_COUPON.name().equals(s.getGoodsSku().getGoodsType()))
                .count();
        if (eCouponCount == 0) {
            return;
        }
        boolean hasNonECoupon = checked.stream()
                .anyMatch(s -> !GoodsTypeEnum.E_COUPON.name().equals(s.getGoodsSku().getGoodsType()));
        if (hasNonECoupon) {
            throw new ServiceException(ResultCode.CARD_KEY_MIXED_CHECKOUT);
        }
        if (CartTypeEnum.CART.equals(tradeDTO.getCartTypeEnum())) {
            throw new ServiceException(ResultCode.CARD_KEY_E_COUPON_CART_FORBIDDEN);
        }
        if (checked.size() > 1) {
            throw new ServiceException(ResultCode.CARD_KEY_MULTI_E_COUPON_SKU);
        }
        CartSkuVO eCouponSku = checked.get(0);
        validateStoreOpenForSell(eCouponSku.getStoreId(), storeService);
        int num = eCouponSku.getNum() == null ? 0 : eCouponSku.getNum();
        int maxAllowed = Math.min(stockSupplier.getAsInt(), MAX_BUY_QUANTITY);
        if (num <= 0 || num > maxAllowed) {
            throw new ServiceException(ResultCode.CARD_KEY_POOL_INSUFFICIENT);
        }
    }

    /**
     * 加入购物车前校验电子卡券限制。
     */
    public static void validateAddToCart(String skuId,
                                         Integer num,
                                         String cartType,
                                         GoodsSkuService goodsSkuService,
                                         StoreService storeService,
                                         IntSupplier stockSupplier) {
        GoodsSku sku = goodsSkuService.getById(skuId);
        if (sku == null || !GoodsTypeEnum.E_COUPON.name().equals(sku.getGoodsType())) {
            return;
        }
        if (CartTypeEnum.CART.name().equals(cartType)) {
            throw new ServiceException(ResultCode.CARD_KEY_E_COUPON_CART_FORBIDDEN);
        }
        if (num != null && num > Math.min(stockSupplier.getAsInt(), MAX_BUY_QUANTITY)) {
            throw new ServiceException(ResultCode.CARD_KEY_POOL_INSUFFICIENT);
        }
        validateStoreOpenForSell(sku.getStoreId(), storeService);
    }

    /**
     * 订单支付前校验店铺是否开启。
     */
    public static void validatePayOrder(Order order, StoreService storeService) {
        if (order == null || !OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            return;
        }
        validateStoreOpenForSell(order.getStoreId(), storeService);
    }

    /**
     * 校验店铺在售状态。
     */
    public static void validateStoreOpenForSell(String storeId, StoreService storeService) {
        Store store = storeService.getById(storeId);
        if (store != null && StoreStatusEnum.CLOSED.name().equals(store.getStoreDisable())) {
            throw new ServiceException(ResultCode.CARD_KEY_STORE_SELL_FORBIDDEN);
        }
    }

    /**
     * 判断是否为满赠电子卡券订单。
     */
    public static boolean isGiftECouponOrder(Order order) {
        return order != null
                && OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())
                && OrderPromotionTypeEnum.GIFT.name().equals(order.getOrderPromotionType());
    }

    /**
     * 判断是否为拼团电子卡券订单。
     */
    public static boolean isPintuanECouponOrder(Order order) {
        return order != null
                && OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())
                && OrderPromotionTypeEnum.PINTUAN.name().equals(order.getOrderPromotionType());
    }

    /**
     * 判断是否为购买型电子卡券订单。
     */
    public static boolean isPurchaseTypeOrder(Order order) {
        if (order == null || !OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            return false;
        }
        if (CharSequenceUtil.isNotBlank(order.getParentOrderSn())) {
            return false;
        }
        String promotionType = order.getOrderPromotionType();
        return OrderPromotionTypeEnum.NORMAL.name().equals(promotionType)
                || OrderPromotionTypeEnum.POINTS.name().equals(promotionType)
                || OrderPromotionTypeEnum.KANJIA.name().equals(promotionType);
    }

    /**
     * 判断订单是否满足电子卡券支付后履约前置门禁。
     */
    public static boolean isECouponFulfillmentEligible(Order order) {
        if (order == null || !OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            return false;
        }
        if (!PayStatusEnum.PAID.name().equals(order.getPayStatus())) {
            return false;
        }
        return OrderStatusEnum.PAID.name().equals(order.getOrderStatus());
    }
}
