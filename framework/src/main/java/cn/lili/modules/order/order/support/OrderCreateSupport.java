package cn.lili.modules.order.order.support;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.CommentStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderComplaintStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.vo.OrderVO;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import cn.lili.modules.promotion.entity.dto.FullDiscountGiftItem;
import cn.lili.modules.promotion.service.GiftCardCashService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import cn.lili.modules.order.order.event.TradeIntoDbEvent;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 订单创建与落库支撑组件
 * <p>
 * 负责购物车结算单（TradeDTO）拆单落库校验、礼品卡扣减抵扣、满赠实物赠品随单落库，以及预约商品结算上下文暂存。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class OrderCreateSupport {

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Autowired
    private GiftCardCashService giftCardCashService;

    @Autowired
    private OrderItemService orderItemService;

    @Autowired
    private OrderLogService orderLogService;

    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;

    public OrderCreateSupport() {
    }

    public OrderCreateSupport(GoodsSkuService goodsSkuService,
                              GiftCardCashService giftCardCashService,
                              OrderItemService orderItemService,
                              OrderLogService orderLogService,
                              ApplicationEventPublisher applicationEventPublisher) {
        this.goodsSkuService = goodsSkuService;
        this.giftCardCashService = giftCardCashService;
        this.orderItemService = orderItemService;
        this.orderLogService = orderLogService;
        this.applicationEventPublisher = applicationEventPublisher;
    }

    /**
     * 校验交易单信息有效性
     *
     * @param tradeDTO     交易单数据传输对象
     * @param orderService 订单服务实例
     */
    public void checkTradeDTO(TradeDTO tradeDTO, OrderService orderService) {
        if (tradeDTO.getParentOrderSn() != null) {
            Order parentOrder = orderService.getBySn(tradeDTO.getParentOrderSn());
            if (parentOrder != null && parentOrder.getMemberId().equals(UserContext.getCurrentUser().getId())) {
                throw new ServiceException(ResultCode.PINTUAN_JOIN_ERROR);
            }
        }
    }

    /**
     * 交易单拆单并落库主业务逻辑
     *
     * @param tradeDTO     交易单传输对象
     * @param orderService 订单服务实例（用于执行批量保存订单）
     */
    public void intoDB(TradeDTO tradeDTO, OrderService orderService) {
        checkTradeDTO(tradeDTO, orderService);
        List<Order> orders = new ArrayList<>(tradeDTO.getCartList().size());
        List<OrderItem> orderItems = new ArrayList<>();
        List<OrderLog> orderLogs = new ArrayList<>();
        List<OrderVO> orderVOS = new ArrayList<>();

        tradeDTO.getCartList().forEach(item -> {
            List<OrderItem> currentOrderItems = new ArrayList<>();
            Order order = new Order(item, tradeDTO);
            OrderVO orderVO = new OrderVO();
            BeanUtil.copyProperties(order, orderVO);
            orders.add(order);
            String message = "订单[" + item.getSn() + "]创建";
            orderLogs.add(new OrderLog(item.getSn(), UserContext.getCurrentUser().getId(),
                    UserContext.getCurrentUser().getRole().getRole(),
                    UserContext.getCurrentUser().getUsername(), message));
            item.getCheckedSkuList().forEach(sku -> {
                orderItems.add(new OrderItem(sku, item, tradeDTO));
                currentOrderItems.add(new OrderItem(sku, item, tradeDTO));
            });
            orderVO.setOrderItems(currentOrderItems);
            orderVOS.add(orderVO);
        });
        tradeDTO.setOrderVO(orderVOS);

        // 礼品卡抵扣/按比例分摊，写入礼品卡使用记录并扣减礼品卡面额
        giftCardCashService.applyGiftCardUsageOnOrderCreate(tradeDTO, orders, orderItems);
        // 满赠实物赠品随单落库（0元赠品且不参与折扣分摊）
        appendPhysicalGiftOrderItems(orders, tradeDTO, orderItems);

        orderService.saveBatch(orders);
        orderItemService.saveBatch(orderItems);
        orderLogService.saveBatch(orderLogs);
        // 发布交易单落库领域事件（解耦驱动预约扩展表暂存与时段占用）
        if (applicationEventPublisher != null) {
            applicationEventPublisher.publishEvent(new TradeIntoDbEvent(this, tradeDTO));
        }
    }

    /**
     * 满优惠活动实物赠品随单落库处理
     * <p>
     * 关键业务规则：
     * <ul>
     *     <li>仅针对普通实物订单（{@link OrderTypeEnum#NORMAL}）且购物车包含实物赠品时执行落库。</li>
     *     <li>赠品行金额属性全部归 0 为 {@link OrderItem}，不参与整单优惠券和积分分摊。</li>
     *     <li>赠品行售后状态置为 {@link OrderItemAfterSaleStatusEnum#EXPIRED} 禁止退款，累加至 {@link Order#getGoodsNum()}。</li>
     *     <li>动态赠品与卡券赠品不产生实物单随单落库，由支付回调直接履约发券或加时。</li>
     * </ul>
     *
     * @param orders     订单主表列表
     * @param tradeDTO   交易单上下文
     * @param orderItems 待批量保存的订单项列表
     */
    public void appendPhysicalGiftOrderItems(List<Order> orders, TradeDTO tradeDTO, List<OrderItem> orderItems) {
        if (tradeDTO == null || CollUtil.isEmpty(tradeDTO.getCartList()) || CollUtil.isEmpty(orders)) {
            return;
        }
        Map<String, Order> orderMap = orders.stream().collect(Collectors.toMap(Order::getSn, o -> o, (a, b) -> a));
        Map<String, OrderVO> orderVOMap = tradeDTO.getOrderVO() == null ? Collections.emptyMap()
                : tradeDTO.getOrderVO().stream().collect(Collectors.toMap(OrderVO::getSn, vo -> vo, (a, b) -> a));

        for (CartVO cartVO : tradeDTO.getCartList()) {
            Order order = orderMap.get(cartVO.getSn());
            if (order == null || !OrderTypeEnum.NORMAL.name().equals(order.getOrderType())) {
                continue;
            }
            Map<String, Integer> requestedQtyMap = new LinkedHashMap<>();
            if (CollUtil.isNotEmpty(cartVO.getGiftItemList())) {
                for (FullDiscountGiftItem giftItem : cartVO.getGiftItemList()) {
                    if (giftItem != null && CharSequenceUtil.isNotBlank(giftItem.getSkuId())) {
                        requestedQtyMap.merge(giftItem.getSkuId().trim(), giftItem.validNum(), Integer::sum);
                    }
                }
            } else if (CollUtil.isNotEmpty(cartVO.getGiftList())) {
                for (String skuId : cartVO.getGiftList()) {
                    if (CharSequenceUtil.isNotBlank(skuId)) {
                        requestedQtyMap.merge(skuId.trim(), 1, Integer::sum);
                    }
                }
            }
            if (requestedQtyMap.isEmpty()) {
                continue;
            }

            List<GoodsSku> giftSkus = goodsSkuService.getGoodsSkuByIdFromCache(new ArrayList<>(requestedQtyMap.keySet()));
            if (CollUtil.isEmpty(giftSkus)) {
                continue;
            }

            OrderVO orderVO = orderVOMap.get(order.getSn());
            int giftTotalNum = 0;

            for (GoodsSku sku : giftSkus) {
                if (sku == null || CharSequenceUtil.isBlank(sku.getId())) {
                    continue;
                }
                boolean isPhysical = sku.getGoodsType() == null
                        || GoodsTypeEnum.PHYSICAL_GOODS.name().equals(sku.getGoodsType());
                if (!isPhysical) {
                    continue;
                }

                int requested = requestedQtyMap.getOrDefault(sku.getId(), 1);
                int available = sku.getQuantity() == null ? Integer.MAX_VALUE : Math.max(0, sku.getQuantity());
                int actual = Math.min(requested, available);
                if (actual <= 0) {
                    log.warn("满赠实物赠品库存不足，随单落库跳过赠送，orderSn={} skuId={} requested={} available={}",
                            order.getSn(), sku.getId(), requested, available);
                    continue;
                }

                PriceDetailDTO giftPriceDetail = new PriceDetailDTO();
                giftPriceDetail.setGoodsPrice(0D);
                giftPriceDetail.setFlowPrice(0D);
                giftPriceDetail.setDiscountPrice(0D);
                giftPriceDetail.setGiftCardPrice(0D);

                OrderItem giftOrderItem = new OrderItem();
                BeanUtil.copyProperties(sku, giftOrderItem, "id", "sn");
                BeanUtil.copyProperties(giftPriceDetail, giftOrderItem, "id");
                giftOrderItem.setSn(SnowFlake.createStr("OI"));
                giftOrderItem.setOrderSn(order.getSn());
                giftOrderItem.setTradeSn(tradeDTO.getSn());
                giftOrderItem.setImage(sku.getThumbnail());
                giftOrderItem.setGoodsName(sku.getGoodsName());
                giftOrderItem.setSkuId(sku.getId());
                giftOrderItem.setNum(actual);
                if (CharSequenceUtil.isNotBlank(sku.getCategoryPath())) {
                    giftOrderItem.setCategoryId(sku.getCategoryPath().substring(
                            sku.getCategoryPath().lastIndexOf(",") + 1
                    ));
                }
                giftOrderItem.setGoodsPrice(sku.getPrice());
                giftOrderItem.setUnitPrice(0D);
                giftOrderItem.setFlowPrice(0D);
                giftOrderItem.setSubTotal(0D);
                giftOrderItem.setPromotionType("GIFT");
                if (cartVO.getFullDiscount() != null && CharSequenceUtil.isNotBlank(cartVO.getFullDiscount().getId())) {
                    giftOrderItem.setPromotionId(cartVO.getFullDiscount().getId());
                }
                giftOrderItem.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.EXPIRED.name());
                giftOrderItem.setCommentStatus(CommentStatusEnum.NEW.name());
                giftOrderItem.setComplainStatus(OrderComplaintStatusEnum.NEW.name());
                giftOrderItem.setPriceDetailDTO(giftPriceDetail);

                orderItems.add(giftOrderItem);
                if (orderVO != null && orderVO.getOrderItems() != null) {
                    orderVO.getOrderItems().add(giftOrderItem);
                }
                giftTotalNum += actual;
            }

            if (giftTotalNum > 0) {
                int baseNum = order.getGoodsNum() == null ? 0 : order.getGoodsNum();
                order.setGoodsNum(baseNum + giftTotalNum);
                if (orderVO != null) {
                    orderVO.setGoodsNum(order.getGoodsNum());
                }
            }
        }
    }
}
