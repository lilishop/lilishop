package cn.lili.modules.order.order.support;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.serviceimpl.ECouponFulfillmentService;
import cn.lili.modules.promotion.entity.dos.Pintuan;
import cn.lili.modules.promotion.mapper.PintuanMapper;
import cn.lili.trigger.enums.DelayTypeEnums;
import cn.lili.trigger.interfaces.TimeTrigger;
import cn.lili.trigger.message.PintuanOrderMessage;
import cn.lili.trigger.model.TimeExecuteConstant;
import cn.lili.trigger.model.TimeTriggerMsg;
import cn.lili.trigger.util.DelayQueueTools;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 拼团订单生命周期流转支撑组件
 * <p>
 * 负责拼团成团聚合判定、模拟成团（虚拟成团补单）、开团24小时延迟任务调度、拼团成功订单状态批量流转，以及拼团超时失败退款兜底。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class OrderPintuanSupport {

    private final PintuanMapper pintuanMapper;
    private final TimeTrigger timeTrigger;
    private final RocketmqCustomProperties rocketmqCustomProperties;
    private final ECouponFulfillmentService eCouponFulfillmentService;
    private final ECouponOrderLifecycleSupport eCouponOrderLifecycleSupport;
    private final OrderStatusSupport orderStatusSupport;

    /**
     * 按主键查询拼团配置，统一通过必需的Mapper读取，避免Service与Mapper备用路径。
     *
     * @param pintuanId 拼团活动ID
     * @return 拼团配置，不存在时为null
     */
    private Pintuan getPintuanById(String pintuanId) {
        return pintuanMapper.selectById(pintuanId);
    }

    /**
     * 注入必需的拼团存储、调度及订单协作组件，卡券履约不经过完整卡密服务。
     *
     * @param pintuanMapper 拼团配置Mapper
     * @param timeTrigger 延时任务调度器
     * @param rocketmqCustomProperties 消息主题配置
     * @param eCouponFulfillmentService 电子卡券履约编排服务
     * @param eCouponOrderLifecycleSupport 电子卡券订单生命周期组件
     * @param orderStatusSupport 订单状态流转支持类
     */
    public OrderPintuanSupport(PintuanMapper pintuanMapper,
                               TimeTrigger timeTrigger,
                               RocketmqCustomProperties rocketmqCustomProperties,
                               ECouponFulfillmentService eCouponFulfillmentService,
                               ECouponOrderLifecycleSupport eCouponOrderLifecycleSupport,
                               OrderStatusSupport orderStatusSupport) {
        this.pintuanMapper = pintuanMapper;
        this.timeTrigger = timeTrigger;
        this.rocketmqCustomProperties = rocketmqCustomProperties;
        this.eCouponFulfillmentService = eCouponFulfillmentService;
        this.eCouponOrderLifecycleSupport = eCouponOrderLifecycleSupport;
        this.orderStatusSupport = orderStatusSupport;
    }

    /**
     * 获取拼团关联的同组已支付订单列表
     *
     * @param pintuanId     拼团活动ID
     * @param parentOrderSn 拼团父单编号
     * @param orderService  订单服务接口
     * @return 关联的已支付订单列表
     */
    public List<Order> getPintuanOrder(String pintuanId, String parentOrderSn, OrderService orderService) {
        LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Order::getPromotionId, pintuanId)
                .eq(Order::getOrderPromotionType, OrderPromotionTypeEnum.PINTUAN.name())
                .eq(Order::getPayStatus, PayStatusEnum.PAID.name());
        queryWrapper.and(i -> i.eq(Order::getSn, parentOrderSn)
                .or(j -> j.eq(Order::getParentOrderSn, parentOrderSn)));
        queryWrapper.ne(Order::getOrderStatus, OrderStatusEnum.CANCELLED.name());
        return orderService.list(queryWrapper);
    }

    /**
     * 聚合判定拼团订单状态（根据是否开启虚拟成团自动流转成功或失败）
     *
     * @param pintuanId     拼团活动ID
     * @param parentOrderSn 拼团父单编号
     * @param orderService  订单服务接口
     */
    public void agglomeratePintuanOrder(String pintuanId, String parentOrderSn, OrderService orderService) {
        Pintuan pintuan = getPintuanById(pintuanId);
        List<Order> list = getPintuanOrder(pintuanId, parentOrderSn, orderService);
        if (Boolean.TRUE.equals(pintuan.getFictitious()) && pintuan.getRequiredNum() > list.size()) {
            pintuanOrderSuccess(list, orderService);
        } else if (Boolean.FALSE.equals(pintuan.getFictitious()) && pintuan.getRequiredNum() > list.size()) {
            pintuanOrderFailed(parentOrderSn, orderService);
        }
    }

    /**
     * 校验开团或参团订单状态，判定是否满足成团人数或需要设定延时任务
     *
     * @param pintuanId     拼团活动ID
     * @param parentOrderSn 拼团父单编号
     * @param orderService  订单服务接口
     */
    public void checkPintuanOrder(String pintuanId, String parentOrderSn, OrderService orderService) {
        Pintuan pintuan = getPintuanById(pintuanId);
        List<Order> list = getPintuanOrder(pintuanId, parentOrderSn, orderService);
        int count = list.size();
        if (count == 1) {
            PintuanOrderMessage pintuanOrderMessage = new PintuanOrderMessage();
            long startTime = DateUtil.offsetHour(new Date(), 24).getTime();
            if (DateUtil.compare(DateUtil.offsetHour(pintuan.getStartTime(), 24), pintuan.getEndTime()) > 0) {
                startTime = pintuan.getEndTime().getTime();
            }
            pintuanOrderMessage.setOrderSn(parentOrderSn);
            pintuanOrderMessage.setPintuanId(pintuanId);
            TimeTriggerMsg timeTriggerMsg = new TimeTriggerMsg(TimeExecuteConstant.PROMOTION_EXECUTOR,
                    startTime,
                    pintuanOrderMessage,
                    DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PINTUAN_ORDER, (pintuanId + parentOrderSn)),
                    rocketmqCustomProperties.getPromotionTopic());

            this.timeTrigger.addDelay(timeTriggerMsg);
        }
        if (pintuan.getRequiredNum() <= count) {
            pintuanOrderSuccess(list, orderService);
        }
    }

    /**
     * 拼团成功批量流转订单状态
     *
     * @param orderList    待处理成团订单列表
     * @param orderService 订单服务接口
     */
    public void pintuanOrderSuccess(List<Order> orderList, OrderService orderService) {
        for (Order order : orderList) {
            if (OrderTypeEnum.VIRTUAL.name().equals(order.getOrderType())) {
                orderStatusSupport.virtualOrderConfirm(order.getSn(), orderService);
            } else if (OrderTypeEnum.NORMAL.name().equals(order.getOrderType())) {
                orderStatusSupport.normalOrderConfirm(order.getSn(), orderService);
            } else if (OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
                eCouponFulfillmentService.confirmReservationAndComplete(order.getSn());
            }
        }
    }

    /**
     * 拼团失败超时关单与退款处理
     *
     * @param parentOrderSn 拼团父单编号
     * @param orderService  订单服务接口
     */
    public void pintuanOrderFailed(String parentOrderSn, OrderService orderService) {
        List<Order> list = orderService.list(new LambdaQueryWrapper<Order>()
                .eq(Order::getParentOrderSn, parentOrderSn).or().eq(Order::getSn, parentOrderSn));
        for (Order order : list) {
            try {
                if (OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
                    eCouponFulfillmentService.releaseReservation(order.getSn());
                    eCouponOrderLifecycleSupport.cancel(order.getSn(), "拼团人数不足，拼团失败！", true);
                } else {
                    orderService.systemCancel(order.getSn(), "拼团人数不足，拼团失败！", true);
                }
            } catch (Exception e) {
                log.error("拼团订单取消失败", e);
            }
        }
    }

    /**
     * 检查并执行虚拟成团补单流程
     *
     * @param pintuanId    拼团活动ID
     * @param requiredNum  成团人数
     * @param fictitious   是否开启虚拟成团
     * @param orderService 订单服务接口
     * @return 是否成功触发成团处理
     */
    public boolean checkFictitiousOrder(String pintuanId, Integer requiredNum, Boolean fictitious, OrderService orderService) {
        List<Order> list = orderService.queryListByPromotion(pintuanId);
        Map<String, List<Order>> map = list.stream().collect(Collectors.groupingBy(Order::getParentOrderSn));
        for (Map.Entry<String, List<Order>> entry : map.entrySet()) {
            if (Boolean.TRUE.equals(fictitious)) {
                fictitiousPintuan(entry, requiredNum, orderService);
            } else {
                for (Order order : entry.getValue()) {
                    if (OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
                        eCouponFulfillmentService.releaseReservation(order.getSn());
                        eCouponOrderLifecycleSupport.cancel(order.getSn(), "拼团人数不足，拼团失败！", true);
                    } else {
                        orderService.systemCancel(order.getSn(), "拼团人数不足，拼团失败！", true);
                    }
                }
            }
        }
        return true;
    }

    /**
     * 执行虚拟成团补单与自动推进
     *
     * @param entry        拼团组与订单列表映射
     * @param requiredNum  所需人数
     * @param orderService 订单服务接口
     */
    public void fictitiousPintuan(Map.Entry<String, List<Order>> entry, Integer requiredNum, OrderService orderService) {
        Map<String, List<Order>> listMap = entry.getValue().stream().collect(Collectors.groupingBy(Order::getPayStatus));
        List<Order> unpaidOrders = listMap.get(PayStatusEnum.UNPAID.name());
        if (unpaidOrders != null && !unpaidOrders.isEmpty()) {
            for (Order unpaidOrder : unpaidOrders) {
                orderService.systemCancel(unpaidOrder.getSn(), "拼团活动结束未付款，系统自动取消！", false);
            }
        }
        List<Order> paidOrders = listMap.get(PayStatusEnum.PAID.name());
        if (paidOrders != null && !paidOrders.isEmpty()) {
            int waitNum = requiredNum - paidOrders.size();
            for (int i = 0; i < waitNum; i++) {
                Order order = new Order();
                BeanUtil.copyProperties(paidOrders.get(0), order, "id", "sn");
                order.setSn(SnowFlake.createStr("G"));
                order.setParentOrderSn(paidOrders.get(0).getParentOrderSn());
                order.setMemberId("-1");
                order.setMemberName("匿名人员");
                order.setDeleteFlag(true);
                orderService.save(order);
                paidOrders.add(order);
            }
            for (Order paidOrder : paidOrders) {
                if (!CharSequenceUtil.equalsAny(paidOrder.getOrderStatus(), OrderStatusEnum.COMPLETED.name(), OrderStatusEnum.DELIVERED.name(),
                        OrderStatusEnum.TAKE.name(), OrderStatusEnum.STAY_PICKED_UP.name())) {
                    if (OrderTypeEnum.NORMAL.name().equals(paidOrder.getOrderType())) {
                        paidOrder.setOrderStatus(OrderStatusEnum.UNDELIVERED.name());
                    } else if (OrderTypeEnum.VIRTUAL.name().equals(paidOrder.getOrderType())) {
                        paidOrder.setOrderStatus(OrderStatusEnum.TAKE.name());
                    }
                    orderService.updateById(paidOrder);
                    orderStatusSupport.orderStatusMessage(paidOrder, orderService);
                }
            }
        }
    }
}
