package cn.lili.modules.order.order.serviceimpl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.aop.syslog.annotation.SystemLogPoint;
import cn.lili.common.delayqueue.DelayQueueTools;
import cn.lili.common.delayqueue.DelayQueueType;
import cn.lili.common.delayqueue.PintuanOrderMessage;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.common.rocketmq.tags.GoodsTagsEnum;
import cn.lili.common.rocketmq.tags.MqOrderTagsEnum;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.trigger.interfaces.TimeTrigger;
import cn.lili.common.trigger.model.TimeExecuteConstant;
import cn.lili.common.trigger.model.TimeTriggerMsg;
import cn.lili.common.utils.*;
import cn.lili.common.vo.PageVO;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsCompleteMessage;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dto.MemberAddressDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.aop.OrderLogPoint;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dos.Receipt;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.*;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.order.order.entity.vo.OrderVO;
import cn.lili.modules.order.order.mapper.OrderItemMapper;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.service.ReceiptService;
import cn.lili.modules.order.order.service.StoreFlowService;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import cn.lili.modules.payment.kit.enums.PaymentMethodEnum;
import cn.lili.modules.promotion.entity.dos.Pintuan;
import cn.lili.modules.promotion.service.PintuanService;
import cn.lili.modules.statistics.model.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import cn.lili.modules.system.entity.dos.Logistics;
import cn.lili.modules.system.entity.vo.Traces;
import cn.lili.modules.system.service.LogisticsService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 子订单业务层实现
 *
 * @author Chopper
 * @date 2020/11/17 7:38 下午
 */
@Service
@Transactional(rollbackFor = Exception.class)
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class OrderServiceImpl extends ServiceImpl<OrderMapper, Order> implements OrderService {

    private static final String ORDER_SN_COLUMN = "order_sn";

    //订单数据层
    private final OrderMapper orderMapper;

    //延时任务
    private final TimeTrigger timeTrigger;

    //订单货物数据层
    private final OrderItemMapper orderItemMapper;
    //发票
    @Autowired
    private ReceiptService receiptService;
    //订单货物
    @Autowired
    private OrderItemService orderItemService;
    //物流公司
    @Autowired
    private LogisticsService logisticsService;
    //订单日志
    @Autowired
    private OrderLogService orderLogService;
    //RocketMQ
    @Autowired
    private RocketMQTemplate rocketMQTemplate;
    //RocketMQ配置
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    //订单流水
    @Autowired
    private StoreFlowService storeFlowService;
    //拼团
    @Autowired
    private PintuanService pintuanService;
    //规格商品
    @Autowired
    private GoodsSkuService goodsSkuService;

    @Override
    public void intoDB(TradeDTO tradeDTO) {
        List<Order> orders = new ArrayList<>(tradeDTO.getCartList().size());
        List<OrderItem> orderItems = new ArrayList<>();
        List<OrderLog> orderLogs = new ArrayList<>();
        if (tradeDTO.getParentOrderSn() != null) {
            Order parentOrder = this.getBySn(tradeDTO.getParentOrderSn());
            if (parentOrder.getMemberId().equals(UserContext.getCurrentUser().getId())) {
                throw new ServiceException("不能参与自己发起的拼团活动！");
            }
        }
        List<OrderVO> list = new ArrayList<>();
        tradeDTO.getCartList().forEach(item -> {
            Order order = new Order(item, tradeDTO);
            if (OrderTypeEnum.PINTUAN.name().equals(order.getOrderType())) {
                Pintuan pintuan = pintuanService.getPintuanById(order.getPromotionId());
                Integer limitNum = pintuan.getLimitNum();
                if (limitNum != 0 && order.getGoodsNum() > limitNum) {
                    throw new ServiceException("购买数量超过拼团活动限制数量");
                }
            }
            //构建orderVO对象
            OrderVO orderVO = new OrderVO();
            BeanUtil.copyProperties(order, orderVO);
            orders.add(order);
            String message = "订单[" + item.getSn() + "]创建";
            orderLogs.add(new OrderLog(item.getSn(), UserContext.getCurrentUser().getId(), UserContext.getCurrentUser().getRole().getRole(), UserContext.getCurrentUser().getUsername(), message));
            item.getSkuList().forEach(
                    sku -> orderItems.add(new OrderItem(sku, item, tradeDTO))
            );
            orderVO.setOrderItems(orderItems);
            list.add(orderVO);
        });
        tradeDTO.setOrderVO(list);
        //批量保存订单
        this.saveBatch(orders);
        //批量保存 子订单
        orderItemService.saveBatch(orderItems);
        // 批量记录订单操作日志
        orderLogService.saveBatch(orderLogs);
        // 赠品根据店铺单独生成订单
        this.generatorGiftOrder(tradeDTO);
    }

    @Override
    public IPage<OrderSimpleVO> queryByParams(OrderSearchParams orderSearchParams) {
        return this.baseMapper.queryByParams(PageUtil.initPage(orderSearchParams), orderSearchParams.queryWrapper());
    }

    @Override
    public OrderDetailVO queryDetail(String orderSn) {
        Order order = this.getBySn(orderSn);
        if (order == null) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        QueryWrapper<OrderItem> orderItemWrapper = new QueryWrapper<>();
        orderItemWrapper.eq(ORDER_SN_COLUMN, orderSn);
        //查询订单项信息
        List<OrderItem> orderItems = orderItemMapper.selectList(orderItemWrapper);
        //查询订单日志信息
        List<OrderLog> orderLogs = orderLogService.getOrderLog(orderSn);
        //查询发票信息
        Receipt receipt = receiptService.getByOrderSn(orderSn);
        //查询订单和自订单，然后写入vo返回
        return new OrderDetailVO(order, orderItems, orderLogs, receipt);
    }

    @Override
    @OrderLogPoint(description = "'订单['+#orderSn+']取消，原因为：'+#reason", orderSn = "#orderSn")
    public Order cancel(String orderSn, String reason) {
        Order order = OperationalJudgment.judgment(this.getBySn(orderSn));
        if (order.getOrderType().equals(OrderTypeEnum.PINTUAN.name()) && !order.getOrderStatus().equals(OrderStatusEnum.UNDELIVERED.name())) {
            throw new ServiceException("未成团订单不可取消");
        }
        if (CharSequenceUtil.equalsAny(order.getOrderStatus(),
                OrderStatusEnum.UNDELIVERED.name(),
                OrderStatusEnum.UNPAID.name(),
                OrderStatusEnum.PAID.name())) {

            order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
            order.setCancelReason(reason);
            //修改订单
            this.updateById(order);
            orderStatusMessage(order);
            return order;
        } else {
            throw new ServiceException("当前订单状态不可取消");
        }
    }


    @Override
    @OrderLogPoint(description = "'订单['+#orderSn+']系统取消，原因为：'+#reason", orderSn = "#orderSn")
    public void systemCancel(String orderSn, String reason) {
        Order order = this.getBySn(orderSn);
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        order.setCancelReason(reason);
        this.updateById(order);
        orderStatusMessage(order);
    }

    /**
     * 获取订单
     *
     * @param orderSn 订单编号
     * @return 订单详情
     */
    @Override
    public Order getBySn(String orderSn) {
        QueryWrapper<Order> orderWrapper = new QueryWrapper<>();
        orderWrapper.eq("sn", orderSn);
        return this.getOne(orderWrapper);
    }

    @Override
    public void payOrder(String orderSn, String paymentMethod, String receivableNo) {

        Order order = this.getBySn(orderSn);
        //如果订单已支付，就不能再次进行支付
        if (order.getPayStatus().equals(PayStatusEnum.PAID.name())) {
            throw new ServiceException(ResultCode.PAY_DOUBLE_ERROR);
        }

        //修改订单状态
        order.setPaymentTime(new Date());
        order.setPaymentMethod(paymentMethod);
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setOrderStatus(OrderStatusEnum.PAID.name());
        order.setReceivableNo(receivableNo);
        this.updateById(order);

        //记录订单流水
        storeFlowService.payOrder(orderSn);

        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setOrderSn(order.getSn());
        orderMessage.setPaymentMethod(paymentMethod);
        orderMessage.setNewStatus(OrderStatusEnum.PAID);
        this.sendUpdateStatusMessage(orderMessage);

        String message = "订单付款，付款方式[" + PaymentMethodEnum.valueOf(paymentMethod).paymentName() + "]";
        OrderLog orderLog = new OrderLog(orderSn, "-1", UserEnums.SYSTEM.name(), "系统操作", message);
        orderLogService.save(orderLog);


    }

    @Override
    @OrderLogPoint(description = "'库存确认'", orderSn = "#orderSn")
    public void afterOrderConfirm(String orderSn) {
        Order order = this.getBySn(orderSn);
        LambdaUpdateWrapper<Order> orderLambdaUpdateWrapper = new LambdaUpdateWrapper<>();

        //如果为虚拟订单-修改订单状态为待核验
        if (order.getOrderType().equals(OrderTypeEnum.FICTITIOUS.name())) {
            //填写提货码
            String code = CommonUtil.getRandomNum();
            orderLambdaUpdateWrapper.eq(Order::getSn, orderSn);
            orderLambdaUpdateWrapper.set(Order::getQrCode, code);
            orderLambdaUpdateWrapper.set(Order::getOrderStatus, OrderStatusEnum.TAKE.name());
            orderLambdaUpdateWrapper.set(Order::getCanReturn, !PaymentMethodEnum.BANK_TRANSFER.name().equals(order.getPaymentMethod()));

            this.update(orderLambdaUpdateWrapper);

            OrderMessage orderMessage = new OrderMessage();
            orderMessage.setNewStatus(OrderStatusEnum.TAKE);
            orderMessage.setOrderSn(order.getSn());
            this.sendUpdateStatusMessage(orderMessage);
        }
        //如果为商品订单-修改订单状态为待发货
        else {
            orderLambdaUpdateWrapper.eq(Order::getSn, orderSn);
            //拼团订单
            if (order.getOrderType().equals(OrderTypeEnum.PINTUAN.name()) && order.getPromotionId() != null) {
                //校验拼团是否成团 对拼团结果进行处理
                this.checkPintuanOrder(order.getPromotionId(), order.getParentOrderSn());
            } else {
                //普通订单直接修改为代发货状态
                orderLambdaUpdateWrapper.set(Order::getOrderStatus, OrderStatusEnum.UNDELIVERED.name());
                orderLambdaUpdateWrapper.set(Order::getCanReturn, !PaymentMethodEnum.BANK_TRANSFER.name().equals(order.getPaymentMethod()));
                this.update(orderLambdaUpdateWrapper);

                OrderMessage orderMessage = new OrderMessage();
                orderMessage.setNewStatus(OrderStatusEnum.UNDELIVERED);
                orderMessage.setOrderSn(order.getSn());
                this.sendUpdateStatusMessage(orderMessage);
            }
        }

        // 发送当前商品购买完成的信息（用于更新商品数据）
        List<OrderItem> orderItems = orderItemService.getByOrderSn(orderSn);
        List<GoodsCompleteMessage> goodsCompleteMessageList = new ArrayList<>();
        for (OrderItem orderItem : orderItems) {
            GoodsCompleteMessage goodsCompleteMessage = new GoodsCompleteMessage();
            goodsCompleteMessage.setGoodsId(orderItem.getGoodsId());
            goodsCompleteMessage.setSkuId(orderItem.getSkuId());
            goodsCompleteMessage.setBuyNum(orderItem.getNum());
            goodsCompleteMessage.setMemberId(order.getMemberId());
            goodsCompleteMessageList.add(goodsCompleteMessage);
        }
        if (!goodsCompleteMessageList.isEmpty()) {
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.BUY_GOODS_COMPLETE.name();
            //发送订单变更mq消息
            rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(goodsCompleteMessageList), RocketmqSendCallbackBuilder.commonCallback());
        }


    }


    @Override
    @SystemLogPoint(description = "修改订单", customerLog = "'订单[' + #orderSn + ']收货信息修改，修改为'+#memberAddressDTO.consigneeDetail+'")
    public Order updateConsignee(String orderSn, MemberAddressDTO memberAddressDTO) {
        Order order = OperationalJudgment.judgment(this.getBySn(orderSn));

        //要记录之前的收货地址，所以需要以代码方式进行调用 不采用注解
        String message = "订单[" + orderSn + "]收货信息修改，由[" + order.getConsigneeDetail() + "]修改为[" + memberAddressDTO.getConsigneeDetail() + "]";

        BeanUtil.copyProperties(memberAddressDTO, order);// 记录订单操作日志
        this.updateById(order);

        OrderLog orderLog = new OrderLog(orderSn, UserContext.getCurrentUser().getId(), UserContext.getCurrentUser().getRole().getRole(), UserContext.getCurrentUser().getUsername(), message);
        orderLogService.save(orderLog);

        return order;
    }

    @Override
    @OrderLogPoint(description = "'订单['+#orderSn+']发货，发货单号['+#logisticsNo+']'", orderSn = "#orderSn")
    public Order delivery(String orderSn, String logisticsNo, String logisticsId) {
        Order order = OperationalJudgment.judgment(this.getBySn(orderSn));
        //如果订单未发货，并且订单状态值等于待发货
        if (order.getDeliverStatus().equals(DeliverStatusEnum.UNDELIVERED.name()) && order.getOrderStatus().equals(OrderStatusEnum.UNDELIVERED.name())) {
            //获取对应物流
            Logistics logistics = logisticsService.getById(logisticsId);
            if (logistics == null) {
                throw new ServiceException(ResultCode.ORDER_LOGISTICS_ERROR);
            }
            //写入物流信息
            order.setLogisticsCode(logistics.getId());
            order.setLogisticsName(logistics.getName());
            order.setLogisticsNo(logisticsNo);
            order.setLogisticsTime(new Date());
            order.setDeliverStatus(DeliverStatusEnum.DELIVERED.name());
            this.updateById(order);
            //修改订单状态为已发送
            this.updateStatus(orderSn, OrderStatusEnum.DELIVERED);


            //修改订单货物可以进行售后、投诉
            orderItemService.update(new UpdateWrapper<OrderItem>().eq(ORDER_SN_COLUMN, orderSn)
                    .set("after_sale_status", OrderItemAfterSaleStatusEnum.NOT_APPLIED)
                    .set("complain_status", OrderComplaintStatusEnum.NO_APPLY));
            //发送订单状态改变消息
            OrderMessage orderMessage = new OrderMessage();
            orderMessage.setNewStatus(OrderStatusEnum.DELIVERED);
            orderMessage.setOrderSn(order.getSn());
            this.sendUpdateStatusMessage(orderMessage);


        } else {
            throw new ServiceException(ResultCode.ORDER_DELIVER_ERROR);
        }
        return order;
    }

    @Override
    public Traces getTraces(String orderSn) {
        //获取订单信息
        Order order = this.getBySn(orderSn);
        //获取踪迹信息
        return logisticsService.getLogistic(order.getLogisticsCode(), order.getLogisticsNo());
    }

    @Override
    @OrderLogPoint(description = "'订单['+#orderSn+']核销，核销码['+#qrCode+']'", orderSn = "#orderSn")
    public Order take(String orderSn, String qrCode) {
        //是否可以查询到订单
        Order order = OperationalJudgment.judgment(this.getBySn(orderSn));
        //判断是否为虚拟订单
        if (!order.getOrderType().equals(OrderTypeEnum.FICTITIOUS.name())) {
            throw new ServiceException(ResultCode.ORDER_TAKE_ERROR);
        }
        //判断虚拟订单状态
        if (order.getOrderStatus().equals(OrderStatusEnum.TAKE.name())) {
            //判断提货码是否正确\修改订单状态
            if (order.getOrderStatus().equals(OrderStatusEnum.TAKE.name()) && qrCode.equals(order.getQrCode())) {
                order.setOrderStatus(OrderStatusEnum.COMPLETED.name());

                this.updateById(order);

                OrderMessage orderMessage = new OrderMessage();
                orderMessage.setNewStatus(OrderStatusEnum.COMPLETED);
                orderMessage.setOrderSn(order.getSn());
                this.sendUpdateStatusMessage(orderMessage);
            }
            return order;
        }
        throw new ServiceException(ResultCode.ORDER_TAKE_ERROR);
    }

    @Override
    @OrderLogPoint(description = "'订单['+#orderSn+']完成'", orderSn = "#orderSn")
    public void complete(String orderSn) {
        //是否可以查询到订单
        Order order = OperationalJudgment.judgment(this.getBySn(orderSn));

        //修改订单状态为完成
        this.updateStatus(orderSn, OrderStatusEnum.COMPLETED);

        //修改订单货物可以进行评价
        orderItemService.update(new UpdateWrapper<OrderItem>().eq(ORDER_SN_COLUMN, orderSn)
                .set("comment_status", CommentStatusEnum.UNFINISHED));
        //发送订单状态改变消息
        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setNewStatus(OrderStatusEnum.COMPLETED);
        orderMessage.setOrderSn(order.getSn());
        this.sendUpdateStatusMessage(orderMessage);
    }

    @Override
    public List<Order> getByTradeSn(String tradeSn) {
        LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper<>();
        return this.list(queryWrapper.eq(Order::getTradeSn, tradeSn));
    }

    @Override
    public void sendUpdateStatusMessage(OrderMessage orderMessage) {
        String destination = rocketmqCustomProperties.getOrderTopic() + ":" + MqOrderTagsEnum.STATUS_CHANGE.name();
        //发送订单变更mq消息
        rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(orderMessage), RocketmqSendCallbackBuilder.commonCallback());
    }

    @Override
    public void deleteOrder(String sn) {
        Order order = this.getBySn(sn);
        if (order == null) {
            log.error("订单号为" + sn + "的订单不存在！");
            throw new ServiceException();
        }
        LambdaUpdateWrapper<Order> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(Order::getSn, sn).set(Order::getDeleteFlag, true);
        this.update(updateWrapper);
        LambdaUpdateWrapper<OrderItem> orderItemLambdaUpdateWrapper = new LambdaUpdateWrapper<>();
        orderItemLambdaUpdateWrapper.eq(OrderItem::getOrderSn, sn).set(OrderItem::getDeleteFlag, true);
        this.orderItemService.update(orderItemLambdaUpdateWrapper);
    }

    @Override
    public IPage<OrderSimpleVO> getStatistics(StatisticsQueryParam statisticsQueryParam, PageVO pageVO) {

        QueryWrapper<OrderSimpleVO> queryWrapper = new QueryWrapper<>();
        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);
        queryWrapper.between("o.create_time", dates[0], dates[1]);
        queryWrapper.eq(StringUtils.isNotEmpty(statisticsQueryParam.getStoreId()),
                "o.store_id", statisticsQueryParam.getStoreId());

        queryWrapper.eq("o.delete_flag", false);
        queryWrapper.groupBy("o.id");
        queryWrapper.orderByDesc("o.id");
        return orderMapper.queryByParams(PageUtil.initPage(pageVO), queryWrapper);
    }

    @Override
    public Boolean invoice(String sn) {
        //根据订单号查询发票信息
        Receipt receipt = receiptService.getByOrderSn(sn);
        //校验发票信息是否存在
        if (receipt != null) {
            receipt.setReceiptStatus(1);
            return receiptService.updateById(receipt);
        }
        throw new ServiceException(ResultCode.USER_RECEIPT_NOT_EXIST);
    }

    /**
     * 自动成团订单处理
     *
     * @param pintuanId     拼团活动id
     * @param parentOrderSn 拼团订单sn
     */
    @Override
    public void agglomeratePintuanOrder(String pintuanId, String parentOrderSn) {
        //获取拼团配置
        Pintuan pintuan = pintuanService.getPintuanById(pintuanId);
        List<Order> list = this.getPintuanOrder(pintuanId, parentOrderSn);
        if (Boolean.TRUE.equals(pintuan.getFictitious()) && pintuan.getRequiredNum() > list.size()) {
            // 如果开启虚拟成团且当前订单数量不足成团数量，则认为拼团成功
            this.pintuanOrderSuccess(list);
        } else if (Boolean.FALSE.equals(pintuan.getFictitious()) && pintuan.getRequiredNum() > list.size()) {
            // 如果未开启虚拟成团且当前订单数量不足成团数量，则认为拼团失败
            this.pintuanOrderFailed(list);
        }
    }

    /**
     * 订单状态变更消息
     *
     * @param order
     */
    private void orderStatusMessage(Order order) {
        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setOrderSn(order.getSn());
        orderMessage.setNewStatus(OrderStatusEnum.valueOf(order.getOrderStatus()));
        this.sendUpdateStatusMessage(orderMessage);
    }

    /**
     * 此方法只提供内部调用，调用前应该做好权限处理
     * 修改订单状态
     *
     * @param orderSn     订单编号
     * @param orderStatus 订单状态
     */
    private void updateStatus(String orderSn, OrderStatusEnum orderStatus) {
        this.baseMapper.updateStatus(orderStatus.name(), orderSn);
    }

    private void checkPintuanOrder(String pintuanId, String parentOrderSn) {
        //拼团有效参数判定
        if (CharSequenceUtil.isEmpty(parentOrderSn)) {
            return;
        }
        //获取拼团配置
        Pintuan pintuan = pintuanService.getPintuanById(pintuanId);
        List<Order> list = this.getPintuanOrder(pintuanId, parentOrderSn);
        int count = list.size();
        if (count == 1) {
            // 如果为开团订单，则发布一个一小时的延时任务，时间到达后，如果未成团则自动结束（未开启虚拟成团的情况下）
            PintuanOrderMessage pintuanOrderMessage = new PintuanOrderMessage();
            long startTime = DateUtil.offsetHour(new Date(), 1).getTime();
            pintuanOrderMessage.setOrderSn(parentOrderSn);
            pintuanOrderMessage.setPintuanId(pintuanId);
            TimeTriggerMsg timeTriggerMsg = new TimeTriggerMsg(TimeExecuteConstant.PROMOTION_EXECUTOR,
                    startTime,
                    pintuanOrderMessage,
                    DelayQueueTools.wrapperUniqueKey(DelayQueueType.PINTUAN_ORDER, (pintuanId + parentOrderSn)),
                    rocketmqCustomProperties.getPromotionTopic());
            this.timeTrigger.addDelay(timeTriggerMsg, cn.lili.common.utils.DateUtil.getDelayTime(startTime));
        }
        //拼团所需人数，小于等于 参团后的人数，则说明成团，所有订单成团
        if (pintuan.getRequiredNum() <= count) {
            this.pintuanOrderSuccess(list);
        }
    }

    /**
     * 根据拼团活动id和拼团订单sn获取所有当前与当前拼团订单sn相关的订单
     *
     * @param pintuanId     拼团活动id
     * @param parentOrderSn 拼团订单sn
     * @return 所有当前与当前拼团订单sn相关的订单
     */
    private List<Order> getPintuanOrder(String pintuanId, String parentOrderSn) {
        //寻找拼团的所有订单
        LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Order::getPromotionId, pintuanId)
                .eq(Order::getOrderType, OrderTypeEnum.PINTUAN.name())
                .eq(Order::getPayStatus, PayStatusEnum.PAID.name());
        // 拼团sn=开团订单sn 或者 参团订单的开团订单sn
        queryWrapper.and(i -> i.eq(Order::getSn, parentOrderSn)
                .or(j -> j.eq(Order::getParentOrderSn, parentOrderSn)));
        //参团后的订单数（人数）
        return this.list(queryWrapper);
    }

    /**
     * 根据提供的拼团订单列表更新拼团状态为拼团成功
     *
     * @param list 需要更新拼团状态为成功的拼团订单列表
     */
    private void pintuanOrderSuccess(List<Order> list) {
        for (Order order : list) {
            LambdaUpdateWrapper<Order> updateWrapper = new LambdaUpdateWrapper<>();
            updateWrapper.eq(Order::getId, order.getId());
            updateWrapper.set(Order::getOrderStatus, OrderStatusEnum.UNDELIVERED.name());
            updateWrapper.set(Order::getCanReturn, !PaymentMethodEnum.BANK_TRANSFER.name().equals(order.getPaymentMethod()));
            this.update(updateWrapper);
        }
    }

    /**
     * 根据提供的拼团订单列表更新拼团状态为拼团失败
     *
     * @param list 需要更新拼团状态为失败的拼团订单列表
     */
    private void pintuanOrderFailed(List<Order> list) {
        for (Order order : list) {
//            LambdaUpdateWrapper<Order> updateWrapper = new LambdaUpdateWrapper<>();
//            updateWrapper.eq(Order::getId, order.getId());
//            updateWrapper.set(Order::getOrderStatus, OrderStatusEnum.CANCELLED.name());
//            updateWrapper.set(Order::getCancelReason, "拼团人数不足，拼团失败！");
//            this.update(updateWrapper);
            try {
                this.cancel(order.getSn(), "拼团人数不足，拼团失败！");
            } catch (Exception e) {
                log.error("拼团订单取消失败", e);
            }
        }
    }

    /**
     * 生成赠品订单
     *
     * @param tradeDTO 生成订单所需参数
     */
    private void generatorGiftOrder(TradeDTO tradeDTO) {
        List<Order> orders = new ArrayList<>(tradeDTO.getCartList().size());
        List<OrderItem> orderItems = new ArrayList<>();
        List<OrderLog> orderLogs = new ArrayList<>();
        for (CartVO cartVO : tradeDTO.getCartList()) {
            if (cartVO.getGiftList() != null && !cartVO.getGiftList().isEmpty()) {
                Order order = new Order();
                PriceDetailDTO priceDetailDTO = new PriceDetailDTO();
                BeanUtil.copyProperties(cartVO, order, "id");
                BeanUtil.copyProperties(priceDetailDTO, order, "id");
                order.setSn(SnowFlake.createStr("G"));
                order.setTradeSn(tradeDTO.getSn());
                order.setOrderType(OrderTypeEnum.GIFT.name());
                order.setOrderStatus(OrderStatusEnum.UNPAID.name());
                order.setPayStatus(PayStatusEnum.UNPAID.name());
                order.setDeliverStatus(DeliverStatusEnum.UNDELIVERED.name());
                order.setMemberId(tradeDTO.getMemberId());
                order.setMemberName(tradeDTO.getMemberName());
                order.setNeedReceipt(false);
                order.setPriceDetailDTO(priceDetailDTO);
                order.setClientType(tradeDTO.getClientType());

                if (tradeDTO.getMemberAddress() != null) {
                    order.setConsigneeAddressIdPath(tradeDTO.getMemberAddress().getConsigneeAddressIdPath());
                    order.setConsigneeAddressPath(tradeDTO.getMemberAddress().getConsigneeAddressPath());
                    order.setConsigneeDetail(tradeDTO.getMemberAddress().getDetail());
                    order.setConsigneeMobile(tradeDTO.getMemberAddress().getMobile());
                    order.setConsigneeName(tradeDTO.getMemberAddress().getName());
                }
                orders.add(order);
                String message = "赠品订单[" + order.getSn() + "]创建";
                orderLogs.add(new OrderLog(order.getSn(), UserContext.getCurrentUser().getId(), UserContext.getCurrentUser().getRole().getRole(), UserContext.getCurrentUser().getUsername(), message));
                for (String giftGoodsId : cartVO.getGiftList()) {
                    GoodsSku goodsSkuByIdFromCache = goodsSkuService.getGoodsSkuByIdFromCache(giftGoodsId);
                    OrderItem orderItem = new OrderItem();
                    BeanUtil.copyProperties(goodsSkuByIdFromCache, orderItem, "id");
                    BeanUtil.copyProperties(priceDetailDTO, orderItem, "id");
                    orderItem.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.NEW.name());
                    orderItem.setCommentStatus(CommentStatusEnum.NEW.name());
                    orderItem.setComplainStatus(OrderComplaintStatusEnum.NEW.name());
                    orderItem.setNum(cartVO.getGoodsNum());
                    orderItem.setOrderSn(order.getSn());
                    orderItem.setTradeSn(tradeDTO.getSn());
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
            }
        }
        if (!orders.isEmpty()) {
            this.saveBatch(orders);
            orderItemService.saveBatch(orderItems);
            orderLogService.saveBatch(orderLogs);
            for (Order order : orders) {
                OrderMessage orderMessage = new OrderMessage();
                orderMessage.setOrderSn(order.getSn());
                orderMessage.setPaymentMethod(PaymentMethodEnum.BANK_TRANSFER.name());
                orderMessage.setNewStatus(OrderStatusEnum.PAID);
                this.sendUpdateStatusMessage(orderMessage);
            }
        }
    }

    @Autowired
    public void setStoreFlowService(StoreFlowService storeFlowService) {
        this.storeFlowService = storeFlowService;
    }

    @Autowired
    public void setPintuanService(PintuanService pintuanService) {
        this.pintuanService = pintuanService;
    }

    @Autowired
    private void setGoodsSkuService(GoodsSkuService goodsSkuService) {
        this.goodsSkuService = goodsSkuService;
    }

}