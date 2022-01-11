package cn.lili.event.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.SnowFlake;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.event.TradeEvent;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.*;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.rocketmq.tags.OrderTagsEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

/**
 * 订单状态处理类
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
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Autowired
    private RocketMQTemplate rocketMQTemplate;

    @Override
    public void orderCreate(TradeDTO tradeDTO) {

        tradeDTO.getCartList().forEach(
                cartVO -> {
                    //有满减优惠，则记录信息
                    if ((cartVO.getGiftList() != null && !cartVO.getGiftList().isEmpty())
                            || (cartVO.getGiftPoint() != null && cartVO.getGiftPoint() > 0)
                            || (cartVO.getGiftCouponList() != null && !cartVO.getGiftCouponList().isEmpty())) {
                        cache.put(CachePrefix.ORDER.getPrefix() + cartVO.getSn(), JSONUtil.toJsonStr(cartVO));
                    }
                }
        );
    }

    @Override
    public void orderChange(OrderMessage orderMessage) {
        if (orderMessage.getNewStatus().equals(OrderStatusEnum.PAID)) {
            log.debug("满减活动，订单状态操作 {}", CachePrefix.ORDER.getPrefix() + orderMessage.getOrderSn());
            renderGift(JSONUtil.toBean(cache.getString(CachePrefix.ORDER.getPrefix() + orderMessage.getOrderSn()), CartVO.class), orderMessage);
        }
    }

    /**
     * 渲染优惠券信息
     */
    private void renderGift(CartVO cartVO, OrderMessage orderMessage) {
        //没有优惠信息则跳过
        if (cartVO == null) {
            return;
        }
        Order order = orderService.getBySn(orderMessage.getOrderSn());
        //赠送积分判定
        try {
            if (cartVO.getGiftPoint() != null && cartVO.getGiftPoint() > 0) {
                memberService.updateMemberPoint(cartVO.getGiftPoint().longValue(), PointTypeEnum.INCREASE.name(),
                        order.getMemberId(), "订单满优惠赠送积分" + cartVO.getGiftPoint());
            }
        } catch (Exception e) {
            log.error("订单赠送积分异常", e);
        }


        try {
            //优惠券判定
            if (cartVO.getGiftCouponList() != null && !cartVO.getGiftCouponList().isEmpty()) {
                cartVO.getGiftCouponList().forEach(couponId -> memberCouponService.receiveCoupon(couponId, order.getMemberId(), order.getMemberName()));
            }
        } catch (Exception e) {
            log.error("订单赠送优惠券异常", e);
        }

        try {
            //赠品潘迪ing
            if (cartVO.getGiftList() != null && !cartVO.getGiftList().isEmpty()) {
                generatorGiftOrder(cartVO.getGiftList(), order);
            }
        } catch (Exception e) {
            log.error("订单赠送赠品异常", e);
        }
    }

    /**
     * 生成赠品订单
     *
     * @param skuIds      赠品sku信息
     * @param originOrder 赠品原订单信息
     */
    private void generatorGiftOrder(List<String> skuIds, Order originOrder) {
        List<OrderItem> orderItems = new ArrayList<>();
        List<OrderLog> orderLogs = new ArrayList<>();
        Order order = new Order();
        PriceDetailDTO priceDetailDTO = new PriceDetailDTO();
        BeanUtil.copyProperties(originOrder, order, "id");
        BeanUtil.copyProperties(priceDetailDTO, order, "id");
        order.setSn(SnowFlake.createStr("G"));
        order.setOrderType(OrderPromotionTypeEnum.GIFT.name());
        order.setOrderStatus(OrderStatusEnum.UNPAID.name());
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setDeliverStatus(DeliverStatusEnum.UNDELIVERED.name());
        order.setNeedReceipt(false);
        order.setPriceDetailDTO(priceDetailDTO);
        order.setClientType(originOrder.getClientType());
        String message = "赠品订单[" + order.getSn() + "]创建";
        orderLogs.add(new OrderLog(order.getSn(), originOrder.getMemberId(), UserEnums.MEMBER.name(), originOrder.getMemberName(), message));

        for (String skuId : skuIds) {
            GoodsSku goodsSkuByIdFromCache = goodsSkuService.getGoodsSkuByIdFromCache(skuId);
            OrderItem orderItem = new OrderItem();
            BeanUtil.copyProperties(goodsSkuByIdFromCache, orderItem, "id");
            BeanUtil.copyProperties(priceDetailDTO, orderItem, "id");
            orderItem.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.NEW.name());
            orderItem.setCommentStatus(CommentStatusEnum.NEW.name());
            orderItem.setComplainStatus(OrderComplaintStatusEnum.NEW.name());
            orderItem.setNum(1);
            orderItem.setOrderSn(order.getSn());
            orderItem.setImage(goodsSkuByIdFromCache.getThumbnail());
            orderItem.setGoodsName(goodsSkuByIdFromCache.getGoodsName());
            orderItem.setSkuId(goodsSkuByIdFromCache.getId());
            orderItem.setCategoryId(goodsSkuByIdFromCache.getCategoryPath().substring(
                    goodsSkuByIdFromCache.getCategoryPath().lastIndexOf(",") + 1
            ));
            orderItem.setGoodsPrice(goodsSkuByIdFromCache.getPrice());
            orderItem.setPriceDetailDTO(priceDetailDTO);
            orderItems.add(orderItem);
        }
        orderService.save(order);
        orderItemService.saveBatch(orderItems);
        orderLogService.saveBatch(orderLogs);


        //发送订单已付款消息
        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setOrderSn(order.getSn());
        orderMessage.setPaymentMethod(order.getPaymentMethod());
        orderMessage.setNewStatus(OrderStatusEnum.PAID);

        String destination = rocketmqCustomProperties.getOrderTopic() + ":" + OrderTagsEnum.STATUS_CHANGE.name();
        //发送订单变更mq消息
        rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(orderMessage), RocketmqSendCallbackBuilder.commonCallback());


    }
}
