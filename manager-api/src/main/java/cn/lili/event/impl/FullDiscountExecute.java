package cn.lili.event.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.SnowFlake;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.event.TradeEvent;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.message.MessageSendCallbackBuilder;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.CommentStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderComplaintStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import cn.lili.modules.promotion.entity.dto.FullDiscountGiftItem;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.rocketmq.tags.OrderTagsEnum;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 满额活动（满赠）订单状态处理类
 * <p>
 * 关键业务规则：
 * <ul>
 *     <li>支持满赠阶梯内多个赠品 SKU（最多 3 个）及每个赠品 1~10 件赠送数量。</li>
 *     <li>订单支付后生成赠品子订单时，实时检查各赠品 SKU 的可用库存（普通/虚拟商品检查 {@link GoodsSku#getQuantity()}，电子卡券检查 {@link CardKeyService#countUnusedPoolStock(String)}）。</li>
 *     <li>若可用库存小于配置的赠送件数但大于 0，则按实际可用库存部分赠送；若可用库存为 0，则跳过该赠品，不影响同阶梯其他赠品及主订单。</li>
 * </ul>
 *
 * @author Chopper
 * @since 2020-07-03 11:20
 **/
@Slf4j
@Service
public class FullDiscountExecute implements TradeEvent, OrderStatusChangeEvent {

    @Autowired
    private Cache cache;

    @Autowired
    private MemberService memberService;

    @Autowired
    private OrderService orderService;

    @Autowired
    private OrderItemService orderItemService;

    @Autowired
    private OrderLogService orderLogService;

    @Autowired
    private MemberCouponService memberCouponService;

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Autowired
    private CardKeyService cardKeyService;

    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Autowired
    private LiliMessageTemplate liliMessageTemplate;

    @Override
    public void orderCreate(TradeDTO tradeDTO) {
        tradeDTO.getCartList().forEach(cartVO -> {
            // 有满赠商品或历史赠积分/赠券，则记录购物车快照供支付后履约
            if ((cartVO.getGiftItemList() != null && !cartVO.getGiftItemList().isEmpty())
                    || (cartVO.getGiftList() != null && !cartVO.getGiftList().isEmpty())
                    || (cartVO.getGiftPoint() != null && cartVO.getGiftPoint() > 0)
                    || (cartVO.getGiftCouponList() != null && !cartVO.getGiftCouponList().isEmpty())) {
                cache.put(CachePrefix.ORDER.getPrefix() + cartVO.getSn(), JSONUtil.toJsonStr(cartVO));
            }
        });
    }

    @Override
    public void orderChange(OrderMessage orderMessage) {
        if (orderMessage.getNewStatus().equals(OrderStatusEnum.PAID)) {
            log.debug("满优惠活动，订单已支付处理 {}", CachePrefix.ORDER.getPrefix() + orderMessage.getOrderSn());
            String cachedCart = cache.getString(CachePrefix.ORDER.getPrefix() + orderMessage.getOrderSn());
            if (CharSequenceUtil.isNotBlank(cachedCart)) {
                renderGift(JSONUtil.toBean(cachedCart, CartVO.class), orderMessage);
            }
        } else if (orderMessage.getNewStatus().equals(OrderStatusEnum.CANCELLED)) {
            log.debug("满优惠活动，取消订单状态操作 {}", CachePrefix.ORDER.getPrefix() + orderMessage.getOrderSn());
            OrderSearchParams searchParams = new OrderSearchParams();
            searchParams.setParentOrderSn(orderMessage.getOrderSn());
            searchParams.setOrderPromotionType(OrderPromotionTypeEnum.GIFT.name());
            List<Order> orders = orderService.queryListByParams(searchParams);
            if (orders != null && !orders.isEmpty()) {
                for (Order giftOrder : orders) {
                    cardKeyService.voidOnSystemFailure(giftOrder.getSn());
                    orderService.systemCancel(giftOrder.getSn(), "主订单取消，赠送订单自动取消", false);
                }
            }
        }
    }

    /**
     * 渲染并发放满赠权益（赠积分、赠优惠券、赠品子单）
     *
     * @param cartVO       购物车快照
     * @param orderMessage 订单状态变更消息
     */
    private void renderGift(CartVO cartVO, OrderMessage orderMessage) {
        if (cartVO == null) {
            return;
        }
        Order order = orderService.getBySn(orderMessage.getOrderSn());
        // 赠送积分判定（兼容历史活动）
        try {
            if (cartVO.getGiftPoint() != null && cartVO.getGiftPoint() > 0) {
                memberService.updateMemberPoint(cartVO.getGiftPoint().longValue(), PointTypeEnum.INCREASE.name(),
                        order.getMemberId(), "订单满优惠赠送积分" + cartVO.getGiftPoint(),
                        cn.lili.modules.member.entity.enums.PointSourceEnum.FULL_DISCOUNT.name());
            }
        } catch (Exception e) {
            log.error("订单赠送积分异常", e);
        }

        // 优惠券判定（兼容历史活动）
        try {
            if (cartVO.getGiftCouponList() != null && !cartVO.getGiftCouponList().isEmpty()) {
                cartVO.getGiftCouponList().forEach(couponId ->
                        memberCouponService.receiveCoupon(couponId, order.getMemberId(), order.getMemberName()));
            }
        } catch (Exception e) {
            log.error("订单赠送优惠券异常", e);
        }

        // 赠品子单生成（支持多赠品、多件数及库存不足部分赠送）
        try {
            Map<String, Integer> requestedGiftQtyMap = resolveRequestedGiftQuantities(cartVO);
            if (!requestedGiftQtyMap.isEmpty()) {
                generatorGiftOrderWithQuantities(requestedGiftQtyMap, order);
            }
        } catch (Exception e) {
            log.error("订单赠送赠品异常", e);
        }
    }

    /**
     * 从购物车快照提取赠品 SKU ID 与请求赠送件数的映射（兼容新 giftItemList 与旧 giftList）
     *
     * @param cartVO 购物车快照
     * @return SKU ID 到请求赠送数量的映射
     */
    private Map<String, Integer> resolveRequestedGiftQuantities(CartVO cartVO) {
        Map<String, Integer> qtyMap = new LinkedHashMap<>();
        if (CollUtil.isNotEmpty(cartVO.getGiftItemList())) {
            for (FullDiscountGiftItem item : cartVO.getGiftItemList()) {
                if (item != null && CharSequenceUtil.isNotBlank(item.getSkuId())) {
                    qtyMap.merge(item.getSkuId().trim(), item.validNum(), Integer::sum);
                }
            }
        } else if (CollUtil.isNotEmpty(cartVO.getGiftList())) {
            for (String skuId : cartVO.getGiftList()) {
                if (CharSequenceUtil.isNotBlank(skuId)) {
                    qtyMap.merge(skuId.trim(), 1, Integer::sum);
                }
            }
        }
        return qtyMap;
    }

    /**
     * 生成赠品订单（保留单件默认方法签名供兼容调用）
     *
     * @param skuIds      赠品 SKU ID 列表
     * @param originOrder 原始主订单
     */
    private void generatorGiftOrder(List<String> skuIds, Order originOrder) {
        if (CollUtil.isEmpty(skuIds)) {
            return;
        }
        Map<String, Integer> qtyMap = new LinkedHashMap<>();
        for (String skuId : skuIds) {
            if (CharSequenceUtil.isNotBlank(skuId)) {
                qtyMap.merge(skuId.trim(), 1, Integer::sum);
            }
        }
        generatorGiftOrderWithQuantities(qtyMap, originOrder);
    }

    /**
     * 根据赠品 SKU 请求数量及实时可用库存生成赠品子订单
     *
     * @param requestedQtyMap 赠品 SKU ID -> 请求赠送数量
     * @param originOrder     原始主订单
     */
    private void generatorGiftOrderWithQuantities(Map<String, Integer> requestedQtyMap, Order originOrder) {
        List<String> skuIds = new ArrayList<>(requestedQtyMap.keySet());
        List<GoodsSku> goodsSkus = goodsSkuService.getGoodsSkuByIdFromCache(skuIds);
        if (goodsSkus == null || goodsSkus.isEmpty()) {
            log.error("赠品不存在：{}", skuIds);
            return;
        }

        // 实时检查可用库存并计算实际赠送数量（库存不足时按实际可用库存部分赠送，为0则跳过）
        Map<String, Integer> actualQtyMap = new LinkedHashMap<>();
        List<GoodsSku> availableSkus = new ArrayList<>();
        for (GoodsSku sku : goodsSkus) {
            if (sku == null || CharSequenceUtil.isBlank(sku.getId())) {
                continue;
            }
            int requested = requestedQtyMap.getOrDefault(sku.getId(), 1);
            int available = resolveAvailableGiftStock(sku);
            int actual = Math.min(requested, available);
            if (actual <= 0) {
                log.warn("满赠赠品库存不足跳过赠送，orderSn={} skuId={} requested={} available={}",
                        originOrder.getSn(), sku.getId(), requested, available);
                continue;
            }
            if (actual < requested) {
                log.info("满赠赠品库存不足按实际可用数量部分赠送，orderSn={} skuId={} requested={} actual={}",
                        originOrder.getSn(), sku.getId(), requested, actual);
            }
            actualQtyMap.put(sku.getId(), actual);
            availableSkus.add(sku);
        }

        if (availableSkus.isEmpty()) {
            return;
        }

        List<GoodsSku> physicalSkus = availableSkus.stream()
                .filter(goodsSku -> GoodsTypeEnum.PHYSICAL_GOODS.name().equals(goodsSku.getGoodsType()))
                .collect(Collectors.toList());
        List<GoodsSku> virtualSkus = availableSkus.stream()
                .filter(goodsSku -> GoodsTypeEnum.VIRTUAL_GOODS.name().equals(goodsSku.getGoodsType()))
                .collect(Collectors.toList());
        List<GoodsSku> eCouponSkus = availableSkus.stream()
                .filter(goodsSku -> GoodsTypeEnum.E_COUPON.name().equals(goodsSku.getGoodsType()))
                .collect(Collectors.toList());

        // 若主订单是普通实物订单，且已在主订单中随单写入了实物赠品 OrderItem，则跳过生成独立的 G 开头实物赠品子单
        if (OrderTypeEnum.NORMAL.name().equals(originOrder.getOrderType()) && !physicalSkus.isEmpty()) {
            List<OrderItem> mainOrderItems = orderItemService.getByOrderSn(originOrder.getSn());
            Set<String> attachedGiftSkuIds = mainOrderItems == null ? Collections.emptySet() : mainOrderItems.stream()
                    .filter(oi -> "GIFT".equals(oi.getPromotionType())
                            || (oi.getFlowPrice() != null && oi.getFlowPrice() == 0D && CharSequenceUtil.isNotBlank(oi.getPromotionType()) && oi.getPromotionType().contains("GIFT")))
                    .map(OrderItem::getSkuId)
                    .collect(Collectors.toSet());
            physicalSkus.removeIf(sku -> attachedGiftSkuIds.contains(sku.getId()));
        }

        if (!physicalSkus.isEmpty()) {
            giftOrderHandlerWithQuantities(physicalSkus, actualQtyMap, originOrder, OrderTypeEnum.NORMAL);
        }
        if (!virtualSkus.isEmpty()) {
            giftOrderHandlerWithQuantities(virtualSkus, actualQtyMap, originOrder, OrderTypeEnum.VIRTUAL);
        }
        if (!eCouponSkus.isEmpty()) {
            giftOrderHandlerWithQuantities(eCouponSkus, actualQtyMap, originOrder, OrderTypeEnum.E_COUPON);
        }
    }

    /**
     * 获取赠品 SKU 当前实际可赠送库存（电子卡券查卡池可用数，实物/虚拟商品查 SKU 库存；未设置库存时默认充足）
     *
     * @param sku 赠品 SKU
     * @return 可用库存数量
     */
    private int resolveAvailableGiftStock(GoodsSku sku) {
        if (GoodsTypeEnum.E_COUPON.name().equals(sku.getGoodsType()) && cardKeyService != null) {
            return Math.max(0, cardKeyService.countUnusedPoolStock(sku.getId()));
        }
        if (sku.getQuantity() == null) {
            return Integer.MAX_VALUE;
        }
        return Math.max(0, sku.getQuantity());
    }

    /**
     * 赠品订单处理（保留原有 3 参数签名供单元测试及默认 1 件场景调用）
     *
     * @param skuList       赠品列表
     * @param originOrder   原始订单
     * @param orderTypeEnum 订单类型
     */
    private void giftOrderHandler(List<GoodsSku> skuList, Order originOrder, OrderTypeEnum orderTypeEnum) {
        giftOrderHandlerWithQuantities(skuList, null, originOrder, orderTypeEnum);
    }

    /**
     * 赠品订单处理（支持按各赠品实际赠送件数创建子订单项）
     *
     * @param skuList       赠品 SKU 列表
     * @param quantityMap   各 SKU 实际赠送数量映射（为空时默认每项 1 件）
     * @param originOrder   原始主订单
     * @param orderTypeEnum 订单类型
     */
    private void giftOrderHandlerWithQuantities(List<GoodsSku> skuList,
                                                Map<String, Integer> quantityMap,
                                                Order originOrder,
                                                OrderTypeEnum orderTypeEnum) {
        OrderSearchParams searchParams = new OrderSearchParams();
        searchParams.setParentOrderSn(originOrder.getSn());
        searchParams.setOrderPromotionType(OrderPromotionTypeEnum.GIFT.name());
        searchParams.setOrderType(orderTypeEnum.name());
        List<Order> existingGiftOrders = orderService.queryListByParams(searchParams);
        if (existingGiftOrders != null && !existingGiftOrders.isEmpty()) {
            log.info("满赠子单已存在，跳过重复创建，parentSn={} orderType={}", originOrder.getSn(), orderTypeEnum);
            return;
        }

        Order order = new Order();
        List<OrderItem> orderItems = new ArrayList<>();
        List<OrderLog> orderLogs = new ArrayList<>();
        PriceDetailDTO priceDetailDTO = new PriceDetailDTO();
        BeanUtil.copyProperties(originOrder, order, "id");
        BeanUtil.copyProperties(priceDetailDTO, order, "id");
        order.setSn(SnowFlake.createStr("G"));
        order.setParentOrderSn(originOrder.getSn());
        order.setOrderPromotionType(OrderPromotionTypeEnum.GIFT.name());
        // 满赠子单随主单已支付，须 PAID 才能通过 E_COUPON 履约门禁
        order.setOrderStatus(OrderStatusEnum.PAID.name());
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setOrderType(orderTypeEnum.name());
        order.setNeedReceipt(false);
        order.setFlowPrice(0D);
        order.setGoodsPrice(0D);
        order.setPriceDetailDTO(priceDetailDTO);
        order.setClientType(originOrder.getClientType());
        order.applyECouponPersistNormalization();

        int totalNum = 0;
        for (GoodsSku goodsSku : skuList) {
            int giftNum = (quantityMap != null && quantityMap.containsKey(goodsSku.getId()))
                    ? Math.max(1, quantityMap.get(goodsSku.getId()))
                    : 1;
            totalNum += giftNum;

            OrderItem orderItem = new OrderItem();
            BeanUtil.copyProperties(goodsSku, orderItem, "id", "sn");
            BeanUtil.copyProperties(priceDetailDTO, orderItem, "id");
            orderItem.setSn(SnowFlake.createStr("OI"));
            orderItem.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.NEW.name());
            orderItem.setCommentStatus(CommentStatusEnum.NEW.name());
            orderItem.setComplainStatus(OrderComplaintStatusEnum.NEW.name());
            orderItem.setNum(giftNum);
            orderItem.setOrderSn(order.getSn());
            orderItem.setImage(goodsSku.getThumbnail());
            orderItem.setGoodsName(goodsSku.getGoodsName());
            orderItem.setSkuId(goodsSku.getId());
            if (CharSequenceUtil.isNotBlank(goodsSku.getCategoryPath())) {
                orderItem.setCategoryId(goodsSku.getCategoryPath().substring(
                        goodsSku.getCategoryPath().lastIndexOf(",") + 1
                ));
            }
            orderItem.setGoodsPrice(goodsSku.getPrice());
            orderItem.setPriceDetailDTO(priceDetailDTO);
            orderItems.add(orderItem);
        }
        order.setGoodsNum(totalNum);

        String message = "赠品订单[" + order.getSn() + "]创建";
        orderLogs.add(new OrderLog(order.getSn(), originOrder.getMemberId(), UserEnums.MEMBER.name(),
                originOrder.getMemberName(), message));

        orderService.save(order);
        orderItemService.saveBatch(orderItems);
        orderLogService.saveBatch(orderLogs);

        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setOrderSn(order.getSn());
        orderMessage.setPaymentMethod(order.getPaymentMethod());
        orderMessage.setNewStatus(OrderStatusEnum.PAID);

        String destination = rocketmqCustomProperties.getOrderTopic() + ":" + OrderTagsEnum.STATUS_CHANGE.name();
        liliMessageTemplate.asyncSend(destination, JSONUtil.toJsonStr(orderMessage),
                MessageSendCallbackBuilder.commonCallback());
    }
}
