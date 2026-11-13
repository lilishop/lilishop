package cn.lili.modules.order.order.serviceimpl;

import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.SearchVO;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.goods.service.CardKeyService;
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
import cn.lili.modules.system.service.LogisticsService;
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
import cn.lili.common.utils.SpringContextUtil;
import org.springframework.beans.factory.ObjectProvider;
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
 * 采用门面模式（Facade Pattern）提供统一的订单业务入口，内部按单一职责解耦委托给对应的 Support 业务组件：
 * <ul>
 *     <li>{@link OrderCreateSupport}：购物车交易单落库、实物赠品随单关联与结算暂存</li>
 *     <li>{@link OrderQuerySupport}：买家/管理端订单查询、统计、详情装配与 Excel 导出</li>
 *     <li>{@link OrderDeliverySupport}：整单/拆包发货、物流跟踪与批量发货导入导出</li>
 *     <li>{@link OrderVerificationSupport}：自提与虚拟商品核销、预览与台账分页</li>
 *     <li>{@link OrderPintuanSupport}：拼团聚合判定、模拟成团调度与成团/失败流转</li>
 *     <li>{@link OrderStatusSupport}：支付流水记账、买家与系统关单退款分摊及状态 MQ 广播</li>
 * </ul>
 *
 * @author Chopper
 * @since 2020/11/17 7:38 下午
 */
@Service
@Slf4j
public class OrderServiceImpl extends ServiceImpl<OrderMapper, Order> implements OrderService {

    @Autowired
    private PickupCardOrderService pickupCardOrderService;
    @Autowired
    private TimeTrigger timeTrigger;
    @Autowired
    private ReceiptService receiptService;
    @Autowired
    private OrderItemService orderItemService;
    @Autowired
    private ObjectProvider<LogisticsService> logisticsServiceProvider;
    private LogisticsService logisticsService;
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
    private ObjectProvider<CardKeyService> cardKeyServiceProvider;
    private CardKeyService cardKeyService;
    @Autowired
    private ObjectProvider<AppointmentService> appointmentServiceProvider;
    private AppointmentService appointmentService;
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

    private LogisticsService getLogisticsService() {
        if (this.logisticsService != null) {
            return this.logisticsService;
        }
        if (this.logisticsServiceProvider != null) {
            return this.logisticsServiceProvider.getIfAvailable();
        }
        return SpringContextUtil.getBean(LogisticsService.class);
    }

    private CardKeyService getCardKeyService() {
        if (this.cardKeyService != null) {
            return this.cardKeyService;
        }
        if (this.cardKeyServiceProvider != null) {
            return this.cardKeyServiceProvider.getIfAvailable();
        }
        return SpringContextUtil.getBean(CardKeyService.class);
    }

    private AppointmentService getAppointmentService() {
        if (this.appointmentService != null) {
            return this.appointmentService;
        }
        if (this.appointmentServiceProvider != null) {
            return this.appointmentServiceProvider.getIfAvailable();
        }
        return SpringContextUtil.getBean(AppointmentService.class);
    }

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

    public OrderDeliverySupport getOrderDeliverySupport() {
        if (orderDeliverySupport == null) {
            orderDeliverySupport = new OrderDeliverySupport(orderItemService, orderPackageService,
                    orderPackageItemService, getLogisticsService(), orderLogService, storeDetailService, pickupCardOrderService);
        }
        return orderDeliverySupport;
    }

    public OrderVerificationSupport getOrderVerificationSupport() {
        if (orderVerificationSupport == null) {
            orderVerificationSupport = new OrderVerificationSupport(orderItemService, getAppointmentService());
        }
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
    @OrderLogPoint(description = "'订单['+#orderSn+']系统取消，原因为：'+#reason", orderSn = "#orderSn")
    @Transactional(rollbackFor = Exception.class)
    public void systemCancel(String orderSn, String reason, Boolean refundMoney) {
        getOrderStatusSupport().systemCancel(orderSn, reason, refundMoney, this);
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
                    && getCardKeyService().isECouponFulfillmentEligible(order)) {
                getCardKeyService().reserveByOrder(orderSn);
            }
            this.checkPintuanOrder(order.getPromotionId(), parentOrderSn);
            return;
        }
        if (cn.lili.modules.order.order.entity.enums.OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            if (getCardKeyService().isECouponFulfillmentEligible(order)) {
                getCardKeyService().fulfillAfterPayment(orderSn);
            }
            return;
        }
        if (cn.lili.modules.order.order.entity.enums.OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            getAppointmentService().fulfillAfterPayment(orderSn);
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
    @OrderLogPoint(description = "'订单['+#orderSn+']提货，核销码['+#verificationCode+']'", orderSn = "#orderSn")
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
    @OrderLogPoint(description = "'订单['+#orderSn+']完成'", orderSn = "#orderSn")
    @Transactional(rollbackFor = Exception.class)
    public void complete(String orderSn) {
        Order order = OperationalJudgment.judgment(this.getBySn(orderSn));
        complete(order, orderSn);
    }

    @Override
    @OrderLogPoint(description = "'订单['+#orderSn+']完成'", orderSn = "#orderSn")
    @Transactional(rollbackFor = Exception.class)
    public void systemComplete(String orderSn) {
        Order order = this.getBySn(orderSn);
        complete(order, orderSn);
    }

    @Override
    @OrderLogPoint(description = "'电子卡券订单['+#orderSn+']系统完成'", orderSn = "#orderSn")
    @Transactional(rollbackFor = Exception.class)
    public boolean systemCompleteECoupon(String orderSn) {
        return getOrderStatusSupport().systemCompleteECoupon(orderSn, this);
    }

    @Override
    @OrderLogPoint(description = "'电子卡券订单['+#orderSn+']系统取消，原因为：'+#reason", orderSn = "#orderSn")
    @Transactional(rollbackFor = Exception.class)
    public boolean systemCancelECoupon(String orderSn, String reason, Boolean refundMoney) {
        return getOrderStatusSupport().systemCancelECoupon(orderSn, reason, refundMoney, this);
    }

    @Transactional(rollbackFor = Exception.class)
    public void complete(Order order, String orderSn) {
        getOrderStatusSupport().complete(order, orderSn, this);
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
