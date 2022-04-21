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
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
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
import java.util.stream.Collectors;

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
        //如果订单已支付
        if (orderMessage.getNewStatus().equals(OrderStatusEnum.PAID)) {
            log.debug("满减活动，订单状态操作 {}", CachePrefix.ORDER.getPrefix() + orderMessage.getOrderSn());
            renderGift(JSONUtil.toBean(cache.getString(CachePrefix.ORDER.getPrefix() + orderMessage.getOrderSn()), CartVO.class), orderMessage);
        } else if (orderMessage.getNewStatus().equals(OrderStatusEnum.CANCELLED)) {
            log.debug("满减活动，取消订单状态操作 {}", CachePrefix.ORDER.getPrefix() + orderMessage.getOrderSn());
            OrderSearchParams searchParams = new OrderSearchParams();
            searchParams.setParentOrderSn(orderMessage.getOrderSn());
            searchParams.setOrderPromotionType(OrderPromotionTypeEnum.GIFT.name());
            List<Order> orders = orderService.queryListByParams(searchParams);
            if (orders != null && !orders.isEmpty()) {
                orderService.systemCancel(orders.get(0).getSn(),"主订单取消，赠送订单字段自动取消");
            }
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
        //获取赠品列表
        List<GoodsSku> goodsSkus = goodsSkuService.getGoodsSkuByIdFromCache(skuIds);

        //赠品判定
        if (goodsSkus == null || goodsSkus.isEmpty()) {
            log.error("赠品不存在：{}", skuIds);
            return;
        }

        //赠品分类，分为实体商品/虚拟商品/电子卡券
        List<GoodsSku> physicalSkus = goodsSkus.stream().filter(goodsSku -> goodsSku.getGoodsType().equals(GoodsTypeEnum.PHYSICAL_GOODS.name())).collect(Collectors.toList());
        List<GoodsSku> virtualSkus = goodsSkus.stream().filter(goodsSku -> goodsSku.getGoodsType().equals(GoodsTypeEnum.VIRTUAL_GOODS.name())).collect(Collectors.toList());
        List<GoodsSku> eCouponSkus = goodsSkus.stream().filter(goodsSku -> goodsSku.getGoodsType().equals(GoodsTypeEnum.E_COUPON.name())).collect(Collectors.toList());

        //如果赠品不为空，则生成对应的赠品订单
        if (!physicalSkus.isEmpty()) {
            giftOrderHandler(physicalSkus, originOrder, OrderTypeEnum.NORMAL);
        }
        if (!virtualSkus.isEmpty()) {
            giftOrderHandler(virtualSkus, originOrder, OrderTypeEnum.VIRTUAL);
        }
        if (!eCouponSkus.isEmpty()) {
            giftOrderHandler(eCouponSkus, originOrder, OrderTypeEnum.E_COUPON);
        }
    }

    /**
     * 赠品订单处理
     *
     * @param skuList       赠品列表
     * @param originOrder   原始订单
     * @param orderTypeEnum 订单类型
     */
    private void giftOrderHandler(List<GoodsSku> skuList, Order originOrder, OrderTypeEnum orderTypeEnum) {
        //初始化订单对象/订单日志/自订单
        Order order = new Order();
        List<OrderItem> orderItems = new ArrayList<>();
        List<OrderLog> orderLogs = new ArrayList<>();
        //初始化价格详情
        PriceDetailDTO priceDetailDTO = new PriceDetailDTO();
        //复制通用属性
        BeanUtil.copyProperties(originOrder, order, "id");
        BeanUtil.copyProperties(priceDetailDTO, order, "id");
        //生成订单参数
        order.setSn(SnowFlake.createStr("G"));
        order.setParentOrderSn(originOrder.getSn());
        order.setOrderPromotionType(OrderPromotionTypeEnum.GIFT.name());
        order.setOrderStatus(OrderStatusEnum.UNPAID.name());
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setOrderType(orderTypeEnum.name());
        order.setNeedReceipt(false);
        order.setPriceDetailDTO(priceDetailDTO);
        order.setClientType(originOrder.getClientType());
        //订单日志
        String message = "赠品订单[" + order.getSn() + "]创建";
        orderLogs.add(new OrderLog(order.getSn(), originOrder.getMemberId(), UserEnums.MEMBER.name(), originOrder.getMemberName(), message));

        //生成子订单
        for (GoodsSku goodsSku : skuList) {
            OrderItem orderItem = new OrderItem();
            BeanUtil.copyProperties(goodsSku, orderItem, "id");
            BeanUtil.copyProperties(priceDetailDTO, orderItem, "id");
            orderItem.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.NEW.name());
            orderItem.setCommentStatus(CommentStatusEnum.NEW.name());
            orderItem.setComplainStatus(OrderComplaintStatusEnum.NEW.name());
            orderItem.setNum(1);
            orderItem.setOrderSn(order.getSn());
            orderItem.setImage(goodsSku.getThumbnail());
            orderItem.setGoodsName(goodsSku.getGoodsName());
            orderItem.setSkuId(goodsSku.getId());
            orderItem.setCategoryId(goodsSku.getCategoryPath().substring(
                    goodsSku.getCategoryPath().lastIndexOf(",") + 1
            ));
            orderItem.setGoodsPrice(goodsSku.getPrice());
            orderItem.setPriceDetailDTO(priceDetailDTO);
            orderItems.add(orderItem);
        }
        //保存订单
        orderService.save(order);
        orderItemService.saveBatch(orderItems);
        orderLogService.saveBatch(orderLogs);


        //发送订单已付款消息（PS:不在这里处理逻辑是因为期望加交给消费者统一处理库存等等问题）
        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setOrderSn(order.getSn());
        orderMessage.setPaymentMethod(order.getPaymentMethod());
        orderMessage.setNewStatus(OrderStatusEnum.PAID);

        String destination = rocketmqCustomProperties.getOrderTopic() + ":" + OrderTagsEnum.STATUS_CHANGE.name();
        //发送订单变更mq消息
        rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(orderMessage), RocketmqSendCallbackBuilder.commonCallback());
    }
}
