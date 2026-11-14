package cn.lili.modules.order.order.serviceimpl;

import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.SearchVO;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.modules.appointment.support.AppointmentCheckoutSupport;
import cn.lili.modules.goods.support.CardKeyTradeSupport;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.live.entity.vos.LiveRoomVO;
import cn.lili.modules.member.entity.dto.MemberAddressDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.aop.OrderLogPoint;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.*;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.vo.*;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.*;
import cn.lili.modules.order.order.support.*;
import cn.lili.modules.order.trade.service.OrderLogService;
import cn.lili.modules.order.order.mapper.TradeMapper;
import cn.lili.modules.promotion.service.GiftCardCashService;
import cn.lili.modules.promotion.service.PickupCardOrderService;
import cn.lili.modules.store.service.StoreDetailService;
import cn.lili.modules.system.aspect.annotation.SystemLogPoint;
import cn.lili.modules.system.entity.vo.Traces;
import cn.lili.modules.system.service.SettingService;
import cn.lili.trigger.interfaces.TimeTrigger;
import cn.lili.common.properties.RocketmqCustomProperties;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.multipart.MultipartFile;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * 子订单业务实现
 * <p>
 * 提供兼容的订单业务入口，内部委托履约编排服务及各职责组件：
 * <ul>
 *     <li>{@link OrderCreateSupport}：购物车交易单落库、实物赠品随单关联与结算暂存</li>
 *     <li>{@link OrderQuerySupport}：买家/管理端订单查询、统计、详情装配与 Excel 导出</li>
 *     <li>{@link OrderDeliverySupport}：整单/拆包发货、物流跟踪与批量发货导入导出</li>
 *     <li>{@link OrderVerificationSupport}：自提与虚拟商品核销、预览与台账分页</li>
 *     <li>{@link OrderPintuanSupport}：拼团聚合判定、模拟成团调度与成团/失败流转</li>
 *     <li>{@link OrderStatusSupport}：通用订单状态流转及状态 MQ 广播</li>
 *     <li>{@link OrderPaymentSupport}：支付记账与关单退款分摊</li>
 *     <li>{@link OrderLifecycleSupport}：通用订单完成、系统关单及对应操作日志</li>
 *     <li>{@link AppointmentCheckoutSupport}：预约支付后履约，避免依赖完整预约服务</li>
 *     <li>{@link ECouponFulfillmentService}：普通购买、满赠与拼团电子卡券的履约编排</li>
 *     <li>{@link ECouponOrderLifecycleSupport}：电子卡券完成、取消及收尾，统一记录对应操作日志</li>
 * </ul>
 *
 * @author Chopper
 * @since 2020/11/17 7:38 下午
 */
@Service
@Slf4j
public class OrderServiceImpl extends ServiceImpl<OrderMapper, Order> implements OrderService {

    @Autowired
    private ECouponFulfillmentService eCouponFulfillmentService;

    @Autowired
    private ECouponOrderLifecycleSupport eCouponOrderLifecycleSupport;

    @Autowired
    private OrderLifecycleSupport orderLifecycleSupport;

    @Autowired
    private AppointmentCheckoutSupport appointmentCheckoutSupport;

    @Autowired
    private PickupCardOrderService pickupCardOrderService;
    @Autowired
    private TimeTrigger timeTrigger;
    @Autowired
    private ReceiptService receiptService;
    @Autowired
    private OrderItemService orderItemService;
    @Autowired
    private OrderLogService orderLogService;
    @Autowired
    private LiliMessageTemplate liliMessageTemplate;
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    @Autowired
    private TradeMapper tradeMapper;
    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;
    @Autowired
    private StoreDetailService storeDetailService;
    @Autowired
    private OrderPackageService orderPackageService;
    @Autowired
    private OrderPackageItemService orderPackageItemService;
    @Autowired
    private SettingService settingService;
    @Autowired
    private GiftCardCashService giftCardCashService;
    @Autowired
    private GoodsSkuService goodsSkuService;
    @Autowired
    private OrderCreateSupport orderCreateSupport;
    @Autowired
    private OrderQuerySupport orderQuerySupport;
    @Autowired
    private OrderDeliverySupport orderDeliverySupport;
    @Autowired
    private OrderVerificationSupport orderVerificationSupport;
    @Autowired
    private OrderPintuanSupport orderPintuanSupport;
    @Autowired
    private OrderStatusSupport orderStatusSupport;
    @Autowired
    private OrderPaymentSupport orderPaymentSupport;

    public OrderPaymentSupport getOrderPaymentSupport() {
        if (orderPaymentSupport == null) {
            orderPaymentSupport = new OrderPaymentSupport(pickupCardOrderService,
                    giftCardCashService, orderLogService, applicationEventPublisher);
        }
        return orderPaymentSupport;
    }

    public OrderCreateSupport getOrderCreateSupport() {
        if (orderCreateSupport == null) {
            orderCreateSupport = new OrderCreateSupport(goodsSkuService, giftCardCashService,
                    orderItemService, orderLogService, applicationEventPublisher);
        }
        return orderCreateSupport;
    }

    public OrderQuerySupport getOrderQuerySupport() {
        if (orderQuerySupport == null) {
            orderQuerySupport = new OrderQuerySupport(pickupCardOrderService, orderItemService,
                    orderLogService, receiptService, tradeMapper);
        }
        return orderQuerySupport;
    }

    /**
     * 返回必需的订单履约组件，物流依赖由组件自身正常注入，不在运行时手动装配。
     *
     * @return Spring装配的订单履约组件
     */
    public OrderDeliverySupport getOrderDeliverySupport() {
        return orderDeliverySupport;
    }

    public OrderVerificationSupport getOrderVerificationSupport() {
        return orderVerificationSupport;
    }

    /**
     * 返回必需的Spring拼团支持组件，不在业务入口手动创建实例。
     *
     * @return 拼团订单支持组件
     */
    public OrderPintuanSupport getOrderPintuanSupport() {
        return orderPintuanSupport;
    }

    public OrderStatusSupport getOrderStatusSupport() {
        if (orderStatusSupport == null) {
            orderStatusSupport = new OrderStatusSupport(orderItemService, orderLogService,
                    pickupCardOrderService, settingService, liliMessageTemplate, rocketmqCustomProperties,
                    applicationEventPublisher, receiptService, getOrderPaymentSupport());
        }
        return orderStatusSupport;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void intoDB(TradeDTO tradeDTO) {
        getOrderCreateSupport().intoDB(tradeDTO, this);
    }

    @SuppressWarnings("unused")
    private void appendPhysicalGiftOrderItems(List<Order> orders, TradeDTO tradeDTO, List<OrderItem> orderItems) {
        getOrderCreateSupport().appendPhysicalGiftOrderItems(orders, tradeDTO, orderItems);
    }

    @Override
    public IPage<OrderSimpleVO> queryBuyerByParams(OrderSearchParams orderSearchParams) {
        return getOrderQuerySupport().queryBuyerByParams(orderSearchParams, this.baseMapper);
    }

    @Override
    public IPage<OrderSimpleVO> queryManageByParams(OrderSearchParams orderSearchParams) {
        return getOrderQuerySupport().queryManageByParams(orderSearchParams, this.baseMapper);
    }

    @Override
    public OrderNumVO getOrderNumVO(OrderSearchParams orderSearchParams) {
        return getOrderQuerySupport().getOrderNumVO(orderSearchParams, this.baseMapper);
    }

    @Override
    public List<Order> queryListByParams(OrderSearchParams orderSearchParams) {
        return getOrderQuerySupport().queryListByParams(orderSearchParams, this.baseMapper);
    }

    @Override
    public List<Order> queryListByPromotion(String orderPromotionType, String payStatus, String parentOrderSn, String orderSn) {
        return getOrderQuerySupport().queryListByPromotion(orderPromotionType, payStatus, parentOrderSn, orderSn, this.baseMapper);
    }

    @Override
    public long queryCountByPromotion(String orderPromotionType, String payStatus, String parentOrderSn, String orderSn) {
        return getOrderQuerySupport().queryCountByPromotion(orderPromotionType, payStatus, parentOrderSn, orderSn, this.baseMapper);
    }

    @Override
    public List<Order> queryListByPromotion(String pintuanId) {
        return getOrderQuerySupport().queryListByPromotion(pintuanId, this.baseMapper);
    }

    @Override
    public void queryExportOrder(HttpServletResponse response, OrderSearchParams orderSearchParams) {
        getOrderQuerySupport().queryExportOrder(response, orderSearchParams, this.baseMapper);
    }

    @Override
    public OrderDetailVO queryDetail(String orderSn) {
        return getOrderQuerySupport().queryDetail(orderSn, this);
    }

    @Override
    @OrderLogPoint(description = "'订单['+#orderSn+']取消，原因为：'+#reason", orderSn = "#orderSn")
    @Transactional(rollbackFor = Exception.class)
    public Order cancel(String orderSn, String reason) {
        return getOrderStatusSupport().cancel(orderSn, reason, this);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void systemCancel(String orderSn, String reason, Boolean refundMoney) {
        orderLifecycleSupport.systemCancel(orderSn, reason, refundMoney);
    }

    @SuppressWarnings("unused")
    private BigDecimal toMoney(Double value) {
        return getOrderPaymentSupport().toMoney(value);
    }

    @Override
    public Order getBySn(String orderSn) {
        return this.getOne(new LambdaQueryWrapper<Order>().eq(Order::getSn, orderSn));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void payOrder(String orderSn, String paymentMethod, String receivableNo) {
        this.payOrder(orderSn, paymentMethod, receivableNo, null);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void payOrder(String orderSn, String paymentMethod, String receivableNo, Double price) {
        getOrderPaymentSupport().payOrder(orderSn, paymentMethod, receivableNo, price, this);
    }

    @Override
    public void refundDuplicatePayment(Order order, String paymentMethod, String receivableNo) {
        getOrderPaymentSupport().refundDuplicatePayment(order, paymentMethod, receivableNo);
    }

    @Override
    @OrderLogPoint(description = "'订单确认'", orderSn = "#orderSn")
    @Transactional(rollbackFor = Exception.class)
    public void afterOrderConfirm(String orderSn) {
        Order order = this.getBySn(orderSn);
        if (order == null) {
            return;
        }
        if (cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum.PINTUAN.name().equals(order.getOrderPromotionType())) {
            String parentOrderSn = cn.hutool.core.text.CharSequenceUtil.isEmpty(order.getParentOrderSn()) ? orderSn : order.getParentOrderSn();
            if (cn.lili.modules.order.order.entity.enums.OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())
                    && CardKeyTradeSupport.isECouponFulfillmentEligible(order)) {
                eCouponFulfillmentService.reserveByOrder(orderSn);
            }
            this.checkPintuanOrder(order.getPromotionId(), parentOrderSn);
            return;
        }
        if (cn.lili.modules.order.order.entity.enums.OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            if (CardKeyTradeSupport.isECouponFulfillmentEligible(order)) {
                eCouponFulfillmentService.fulfillAfterPayment(orderSn);
            }
            return;
        }
        if (cn.lili.modules.order.order.entity.enums.OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            appointmentCheckoutSupport.fulfillAfterPayment(orderSn);
            return;
        }
        if (cn.lili.modules.order.order.entity.enums.OrderTypeEnum.NORMAL.name().equals(order.getOrderType())) {
            this.normalOrderConfirm(orderSn);
        } else if (cn.lili.modules.order.order.entity.enums.OrderTypeEnum.VIRTUAL.name().equals(order.getOrderType())) {
            this.virtualOrderConfirm(orderSn);
        }
    }

    @Override
    @SystemLogPoint(description = "修改订单", customerLog = "'订单[' + #orderSn + ']收货信息修改，修改为'+#memberAddressDTO.consigneeDetail")
    @Transactional(rollbackFor = Exception.class)
    public Order updateConsignee(String orderSn, MemberAddressDTO memberAddressDTO) {
        return getOrderStatusSupport().updateConsignee(orderSn, memberAddressDTO, this);
    }

    @Override
    @OrderLogPoint(description = "'订单['+#orderSn+']发货，发货单号['+#logisticsNo+']'", orderSn = "#orderSn")
    @Transactional(rollbackFor = Exception.class)
    public Order delivery(String orderSn, String logisticsNo, String logisticsId) {
        return getOrderDeliverySupport().delivery(orderSn, logisticsNo, logisticsId, this, this.baseMapper);
    }

    @Override
    @OrderLogPoint(description = "'订单['+#params.orderSn+']统一发货'", orderSn = "#params.orderSn")
    @Transactional(rollbackFor = Exception.class)
    public Order fulfillOrder(OrderFulfillParamsDTO params) {
        return getOrderDeliverySupport().fulfillOrder(params, this, this.baseMapper);
    }

    @Override
    public Order shunFengDelivery(String orderSn) {
        return getOrderDeliverySupport().shunFengDelivery(orderSn, this, this.baseMapper);
    }

    @Override
    public Traces getTraces(String orderSn) {
        return getOrderDeliverySupport().getTraces(orderSn, this);
    }

    @Override
    public Traces getMapTraces(String orderSn) {
        return getOrderDeliverySupport().getMapTraces(orderSn, this);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Order take(String orderSn, String verificationCode) {
        return getOrderVerificationSupport().take(orderSn, verificationCode, this);
    }

    @Override
    public Order take(String verificationCode) {
        return getOrderVerificationSupport().take(verificationCode, this);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String confirmVerification(String code) {
        return getOrderVerificationSupport().confirmVerification(code, this);
    }

    @Override
    public OrderVerificationPreviewVO previewVerification(String code) {
        return getOrderVerificationSupport().previewVerification(code, this);
    }

    @Override
    public IPage<OrderVerificationRecordVO> verificationRecordPage(OrderVerificationRecordSearchParams params) {
        return getOrderVerificationSupport().verificationRecordPage(params, this.baseMapper);
    }

    @Override
    public Order getOrderByVerificationCode(String verificationCode) {
        return getOrderVerificationSupport().getOrderByVerificationCode(verificationCode, this);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void complete(String orderSn) {
        orderLifecycleSupport.complete(orderSn);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void systemComplete(String orderSn) {
        orderLifecycleSupport.systemComplete(orderSn);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean systemCompleteECoupon(String orderSn) {
        return eCouponOrderLifecycleSupport.complete(orderSn);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean systemCancelECoupon(String orderSn, String reason, Boolean refundMoney) {
        return eCouponOrderLifecycleSupport.cancel(orderSn, reason, refundMoney);
    }

    @Transactional(rollbackFor = Exception.class)
    public void complete(Order order, String orderSn) {
        getOrderStatusSupport().complete(order, orderSn);
    }

    @Override
    public List<Order> getByTradeSn(String tradeSn) {
        return getOrderQuerySupport().getByTradeSn(tradeSn, this.baseMapper);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void sendUpdateStatusMessage(OrderMessage orderMessage) {
        getOrderStatusSupport().sendUpdateStatusMessage(orderMessage);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteOrder(String sn) {
        getOrderStatusSupport().deleteOrder(sn, this);
    }

    @Override
    public Boolean invoice(String sn) {
        return getOrderStatusSupport().invoice(sn);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void agglomeratePintuanOrder(String pintuanId, String parentOrderSn) {
        getOrderPintuanSupport().agglomeratePintuanOrder(pintuanId, parentOrderSn, this);
    }

    @Override
    public void getBatchDeliverList(HttpServletResponse response, List<String> logisticsName) {
        getOrderDeliverySupport().getBatchDeliverList(response, logisticsName);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void batchDeliver(MultipartFile files) {
        getOrderDeliverySupport().batchDeliver(files, this, this.baseMapper);
    }

    @Override
    public Double getPaymentTotal(String orderSn) {
        return getOrderQuerySupport().getPaymentTotal(orderSn, this);
    }

    @Override
    public IPage<PaymentLog> queryPaymentLogs(IPage<PaymentLog> page, Wrapper<PaymentLog> queryWrapper) {
        return getOrderQuerySupport().queryPaymentLogs(page, queryWrapper, this.baseMapper);
    }

    @Override
    public IPage<PaymentLog> queryPaymentLogs(Order order, SearchVO searchVO, PageVO pageVO) {
        return getOrderQuerySupport().queryPaymentLogs(order, searchVO, pageVO, this.baseMapper);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean checkFictitiousOrder(String pintuanId, Integer requiredNum, Boolean fictitious) {
        return getOrderPintuanSupport().checkFictitiousOrder(pintuanId, requiredNum, fictitious, this);
    }

    @Override
    public Order partDelivery(PartDeliveryParamsDTO partDeliveryParamsDTO) {
        return getOrderDeliverySupport().partDelivery(partDeliveryParamsDTO, this, this.baseMapper);
    }

    @Override
    public Order updateSellerRemark(String orderSn, String sellerRemark) {
        return getOrderStatusSupport().updateSellerRemark(orderSn, sellerRemark, this);
    }

    @Override
    public void countLiveOrderData(@RequestBody LiveRoomVO liveRoomVO) {
        getOrderQuerySupport().countLiveOrderData(liveRoomVO, this);
    }

    public void fictitiousPintuan(Map.Entry<String, List<Order>> entry, Integer requiredNum) {
        getOrderPintuanSupport().fictitiousPintuan(entry, requiredNum, this);
    }

    public void orderStatusMessage(Order order) {
        getOrderStatusSupport().orderStatusMessage(order, this);
    }

    public void checkPintuanOrder(String pintuanId, String parentOrderSn) {
        getOrderPintuanSupport().checkPintuanOrder(pintuanId, parentOrderSn, this);
    }

    @Transactional(rollbackFor = Exception.class)
    public void pintuanOrderSuccess(List<Order> orderList) {
        getOrderPintuanSupport().pintuanOrderSuccess(orderList, this);
    }

    @Transactional(rollbackFor = Exception.class)
    public void normalOrderConfirm(String orderSn) {
        getOrderStatusSupport().normalOrderConfirm(orderSn, this);
    }

    @Transactional(rollbackFor = Exception.class)
    public void virtualOrderConfirm(String orderSn) {
        getOrderStatusSupport().virtualOrderConfirm(orderSn, this);
    }
}
