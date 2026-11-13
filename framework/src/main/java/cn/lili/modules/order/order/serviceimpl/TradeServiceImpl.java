package cn.lili.modules.order.order.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.event.TransactionCommitSendMQEvent;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.cart.entity.dto.MemberCouponDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.Trade;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.mapper.TradeMapper;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.service.TradeService;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.KanjiaActivityService;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.rocketmq.tags.OrderTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.utils.SpringContextUtil;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.Payment;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 交易业务层实现，单向调用订单服务完成落库与支付入账，不依赖延迟注入。
 * 创建交易及支付入账沿用当前服务事务，订单处理失败时继续向上传播异常以触发回滚。
 *
 * @author Chopper
 * @since 2020/11/17 7:39 下午
 */
@Slf4j
@Service
public class TradeServiceImpl extends ServiceImpl<TradeMapper, Trade> implements TradeService {

    @Autowired
    private OrderLogService orderLogService;
    @Autowired
    private Cache<Object> cache;
    @Autowired
    private OrderService orderService;
    @Autowired
    private MemberService memberService;
    @Autowired
    private CouponService couponService;
    @Autowired
    private MemberCouponService memberCouponService;
    @Autowired
    private KanjiaActivityService kanjiaActivityService;
    @Autowired
    private CardKeyService cardKeyService;
    @Autowired
    private AppointmentService appointmentService;
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Trade createTrade(TradeDTO tradeDTO) {
        //创建订单预校验
        createTradeCheck(tradeDTO);
        Trade trade = new Trade(tradeDTO);
        String key = CachePrefix.TRADE.getPrefix() + trade.getSn();
        //优惠券预处理
        couponPretreatment(tradeDTO);
        //积分预处理
        pointPretreatment(tradeDTO);
        //添加交易
        this.save(trade);
        //添加订单
        orderService.intoDB(tradeDTO);
        //砍价订单处理
        kanjiaPretreatment(tradeDTO);
        //写入缓存，给消费者调用
        cache.put(key, JSONUtil.toJsonStr(tradeDTO));
        applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("订单创建消息", rocketmqCustomProperties.getOrderTopic(),
                OrderTagsEnum.ORDER_CREATE.name(), key));
        return trade;
    }

    /**
     * 创建交易入库前校验。
     * <ul>
     *   <li>E_COUPON：{@link CardKeyService#validateTradeForECoupon}（混单/促销/卡池/金额等）</li>
     *   <li>收货地址：由 {@link cn.lili.modules.order.cart.service.CartServiceImpl#createTrade} 在渲染前校验</li>
     *   <li>本方法：勾选状态、E_COUPON 跳过后续物流校验、实物配送区域与自提门店</li>
     * </ul>
     *
     * @param tradeDTO 购物车视图
     */
    private void createTradeCheck(TradeDTO tradeDTO) {
        cardKeyService.validateTradeForECoupon(tradeDTO);
        appointmentService.validateTrade(tradeDTO);
        if (tradeDTO.getCartList().stream().noneMatch(CartVO::getChecked)) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST_VALID);
        }
        if (cardKeyService.isECouponOnlyTrade(tradeDTO) || appointmentService.isAppointmentOnlyTrade(tradeDTO)) {
            return;
        }
        if (!tradeDTO.getCartTypeEnum().equals(CartTypeEnum.VIRTUAL) && tradeDTO.getStoreAddress() == null) {
            if (tradeDTO.getNotSupportFreight() != null && !tradeDTO.getNotSupportFreight().isEmpty()) {
                StringBuilder stringBuilder = new StringBuilder("包含商品有-");
                tradeDTO.getNotSupportFreight().forEach(sku -> stringBuilder.append(sku.getGoodsSku().getGoodsName()));
                throw new ServiceException(ResultCode.ORDER_NOT_SUPPORT_DISTRIBUTION, stringBuilder.toString());
            }
            if (tradeDTO.getCartList().stream().allMatch(item -> item.getDeliveryMethod().equals(DeliveryMethodEnum.SELF_PICK_UP.name()))) {
                throw new ServiceException(ResultCode.STORE_ADDRESS_NOT_EXIST);
            }
        }
    }

    @Override
    public Trade getBySn(String sn) {
        LambdaQueryWrapper<Trade> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Trade::getSn, sn);
        return this.getOne(queryWrapper);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void payTrade(String tradeSn, String paymentName, String receivableNo) {
        this.payTrade(tradeSn, paymentName, receivableNo, null);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void payTrade(String tradeSn, String paymentName, String receivableNo, Double price) {
        Trade trade = this.getBySn(tradeSn);
        if (trade == null) {
            log.error("交易[{}]不存在，无法完成支付入账", tradeSn);
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        if (price != null) {
            java.math.BigDecimal paidAmount = java.math.BigDecimal.valueOf(price).setScale(2, java.math.RoundingMode.HALF_UP);
            java.math.BigDecimal tradeAmount = java.math.BigDecimal.valueOf(trade.getFlowPrice()).setScale(2, java.math.RoundingMode.HALF_UP);
            if (paidAmount.compareTo(tradeAmount) != 0) {
                log.error("交易[{}]支付金额不一致，拒绝入账：通知金额={}, 交易应付金额={}",
                        tradeSn, paidAmount, tradeAmount);
                throw new ServiceException(ResultCode.PAY_INCONSISTENT_ERROR);
            }
        }
        LambdaQueryWrapper<Order> orderQueryWrapper = new LambdaQueryWrapper<>();
        orderQueryWrapper.eq(Order::getTradeSn, tradeSn);
        List<Order> orders = orderService.list(orderQueryWrapper);
        for (Order order : orders) {
            orderService.payOrder(order.getSn(), paymentName, receivableNo, order.getFlowPrice());
        }
        trade.setPayStatus(PayStatusEnum.PAID.name());
        this.saveOrUpdate(trade);

    }

    @Override
    public void updateTradePrice(String tradeSn) {
        this.baseMapper.updateTradePrice(tradeSn);
    }

    @Override
    public void refundDuplicatePayment(String tradeSn, String paymentMethod, String receivableNo, Double price) {
        Trade trade = this.getBySn(tradeSn);
        if (trade == null) {
            log.error("交易[{}]不存在，无法执行重复付款原路退款", tradeSn);
            return;
        }
        Double refundAmount = (price != null && price > 0) ? price : trade.getFlowPrice();
        if (refundAmount == null || refundAmount <= 0) {
            log.warn("交易[{}]实付金额为0，无需发起重复付款原路退款", tradeSn);
            return;
        }
        PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.paymentNameOf(paymentMethod);
        if (paymentMethodEnum == null) {
            log.error("交易[{}]重复付款退款失败：未识别支付方式[{}]", tradeSn, paymentMethod);
            return;
        }
        RefundLog refundLog = RefundLog.builder()
                .isRefund(false)
                .totalAmount(refundAmount)
                .payPrice(refundAmount)
                .memberId(trade.getMemberId())
                .paymentName(paymentMethod)
                .afterSaleNo("DUP_PAY_" + SnowFlake.getIdStr())
                .paymentReceivableNo(receivableNo)
                .outOrderNo("RF" + SnowFlake.getIdStr())
                .orderSn(tradeSn)
                .refundReason("交易重复支付，第二笔款项自动原路退回")
                .build();
        try {
            Payment payment = (Payment) SpringContextUtil.getBean(paymentMethodEnum.getPlugin());
            if (payment != null) {
                payment.refund(refundLog);
                log.info("交易[{}]重复付款原路退款成功发起：receivableNo={}, refundAmount={}",
                        tradeSn, receivableNo, refundAmount);
            }
        } catch (Exception e) {
            log.error("交易[{}]重复付款原路退款请求失败：receivableNo={}", tradeSn, receivableNo, e);
        }
        List<Order> orders = orderService.getByTradeSn(tradeSn);
        if (orders != null) {
            for (Order order : orders) {
                String logMsg = "交易已支付，收到重复付款通知[方式: " + paymentMethod + "，流水: " + receivableNo + "]，已自动触发原路退款";
                OrderLog orderLog = new OrderLog(order.getSn(), "-1", UserEnums.SYSTEM.getRole(), "系统操作", logMsg);
                orderLogService.save(orderLog);
            }
        }
    }

    /**
     * 优惠券预处理
     * 下单同时，扣除优惠券
     *
     * @param tradeDTO 购物车视图
     */
    private void couponPretreatment(TradeDTO tradeDTO) {
        List<MemberCouponDTO> memberCouponDTOList = new ArrayList<>();
        if (null != tradeDTO.getPlatformCoupon()) {
            memberCouponDTOList.add(tradeDTO.getPlatformCoupon());
        }
        Collection<MemberCouponDTO> storeCoupons = tradeDTO.getStoreCoupons().values();
        if (!storeCoupons.isEmpty()) {
            memberCouponDTOList.addAll(storeCoupons);
        }
        List<String> ids = memberCouponDTOList.stream().map(e -> e.getMemberCoupon().getId()).collect(Collectors.toList());
        memberCouponService.used(tradeDTO.getMemberId(), ids);
        memberCouponDTOList.forEach(e -> couponService.usedCoupon(e.getMemberCoupon().getCouponId(), 1));
    }

    /**
     * 创建交易，积分处理
     *
     * @param tradeDTO 购物车视图
     */
    private void pointPretreatment(TradeDTO tradeDTO) {
        //需要支付积分
        if (tradeDTO.getPriceDetailDTO() != null
                && tradeDTO.getPriceDetailDTO().getPayPoint() != null
                && tradeDTO.getPriceDetailDTO().getPayPoint() > 0) {
            StringBuilder orderSns = new StringBuilder();
            for (CartVO item : tradeDTO.getCartList()) {
                orderSns.append(item.getSn());
            }
            boolean result = memberService.updateMemberPoint(tradeDTO.getPriceDetailDTO().getPayPoint(), PointTypeEnum.REDUCE.name(),
                    tradeDTO.getMemberId(),
                    "订单【" + orderSns + "】创建，积分扣减",
                    cn.lili.modules.member.entity.enums.PointSourceEnum.ORDER_PAY.name());
            if (!result) {
                throw new ServiceException(ResultCode.PAY_POINT_ENOUGH);
            }
        }
    }

    /**
     * 创建交易、砍价处理
     *
     * @param tradeDTO 购物车视图
     */
    private void kanjiaPretreatment(TradeDTO tradeDTO) {
        if (tradeDTO.getCartTypeEnum().equals(CartTypeEnum.KANJIA)) {
            String kanjiaId = tradeDTO.getSkuList().get(0).getKanjiaId();
            kanjiaActivityService.endKanjiaActivity(kanjiaId);
        }
    }
}
