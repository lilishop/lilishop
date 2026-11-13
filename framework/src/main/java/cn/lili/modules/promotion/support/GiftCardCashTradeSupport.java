package cn.lili.modules.promotion.support;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dos.OrderGiftCardRecord;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.mapper.OrderGiftCardRecordMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.promotion.entity.dos.GiftCardCashCredential;
import cn.lili.modules.promotion.entity.dto.GiftCardCashDeductPreviewDTO;
import cn.lili.modules.promotion.entity.dto.search.GiftCardUsageRecordSearchParams;
import cn.lili.modules.promotion.entity.enums.GiftCardCashCredentialStatusEnum;
import cn.lili.modules.promotion.entity.vos.GiftCardCashDeductPreviewVO;
import cn.lili.modules.promotion.entity.vos.GiftCardUsageRecordVO;
import cn.lili.modules.promotion.mapper.GiftCardCashCredentialMapper;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 现金礼品卡订单抵扣试算、结算扣减、订单分摊、退款回退与交易流水支持类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
public class GiftCardCashTradeSupport {

    /**
     * 试算会员礼品卡抵扣金额。
     *
     * @param credentialMapper 卡密凭证映射
     * @param memberId 会员ID
     * @param dto 抵扣试算入参
     * @return 试算结果对象
     */
    public static GiftCardCashDeductPreviewVO previewMemberCardDeduction(GiftCardCashCredentialMapper credentialMapper,
                                                                         String memberId,
                                                                         GiftCardCashDeductPreviewDTO dto) {
        if (dto == null || dto.getOrderAmount() == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "试算参数错误，orderAmount不能为空");
        }
        BigDecimal orderAmount = dto.getOrderAmount().setScale(2, RoundingMode.HALF_UP);
        if (orderAmount.compareTo(BigDecimal.ZERO) < 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "订单金额不能小于0");
        }
        GiftCardCashDeductPreviewVO result = new GiftCardCashDeductPreviewVO();
        result.setOrderAmount(orderAmount);
        result.setGiftCardDeductAmount(zeroMoney());
        result.setCashPayAmount(orderAmount);
        if (CollUtil.isEmpty(dto.getCredentialIds()) || orderAmount.compareTo(BigDecimal.ZERO) == 0) {
            return result;
        }
        List<String> credentialIds = new ArrayList<>(new LinkedHashSet<>(dto.getCredentialIds()));
        Date now = new Date();
        List<GiftCardCashCredential> selected = credentialMapper.selectList(new LambdaQueryWrapper<GiftCardCashCredential>()
                .in(GiftCardCashCredential::getId, credentialIds)
                .eq(GiftCardCashCredential::getDeleteFlag, false)
                .eq(GiftCardCashCredential::getActivateMemberId, memberId)
                .eq(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.BOUND.name()));
        if (selected.stream().anyMatch(c -> "PICKUP".equals(c.getCardType()))) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "提货卡仅可在专用兑换页使用");
        }
        if (selected.size() != credentialIds.size()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "存在无效礼品卡，无法试算");
        }
        Map<String, GiftCardCashCredential> credentialMap = selected.stream()
                .collect(Collectors.toMap(GiftCardCashCredential::getId, c -> c));
        BigDecimal remainNeed = orderAmount;
        List<GiftCardCashDeductPreviewVO.DeductItem> items = new ArrayList<>();
        for (String credentialId : credentialIds) {
            GiftCardCashCredential credential = credentialMap.get(credentialId);
            if (credential == null) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "存在无效礼品卡，无法试算");
            }
            if (credential.getExpireTime() != null && !credential.getExpireTime().after(now)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "存在已过期礼品卡，无法试算");
            }
            BigDecimal balance = credential.getBalance() == null
                    ? zeroMoney() : credential.getBalance().setScale(2, RoundingMode.HALF_UP);
            if (balance.compareTo(BigDecimal.ZERO) <= 0 || remainNeed.compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            BigDecimal deduct = balance.min(remainNeed).setScale(2, RoundingMode.HALF_UP);
            if (deduct.compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            GiftCardCashDeductPreviewVO.DeductItem item = new GiftCardCashDeductPreviewVO.DeductItem();
            item.setCredentialId(credential.getId());
            item.setCardNo(credential.getCardNo());
            item.setDeductAmount(deduct);
            items.add(item);
            remainNeed = remainNeed.subtract(deduct).setScale(2, RoundingMode.HALF_UP);
        }
        BigDecimal giftCardDeductAmount = orderAmount.subtract(remainNeed).setScale(2, RoundingMode.HALF_UP);
        result.setDeductItems(items);
        result.setGiftCardDeductAmount(giftCardDeductAmount);
        result.setCashPayAmount(orderAmount.subtract(giftCardDeductAmount).setScale(2, RoundingMode.HALF_UP));
        return result;
    }

    /**
     * 扣减会员礼品卡余额。
     *
     * @param credentialMapper 卡密凭证映射
     * @param memberId 会员ID
     * @param deductItems 扣减明细列表
     */
    public static void deductMemberCardBalance(GiftCardCashCredentialMapper credentialMapper,
                                               String memberId,
                                               List<GiftCardCashDeductPreviewVO.DeductItem> deductItems) {
        if (CollUtil.isEmpty(deductItems)) {
            return;
        }
        Date now = new Date();
        for (GiftCardCashDeductPreviewVO.DeductItem item : deductItems) {
            if (item == null || CharSequenceUtil.isBlank(item.getCredentialId()) || item.getDeductAmount() == null) {
                continue;
            }
            BigDecimal needDeduct = item.getDeductAmount().setScale(2, RoundingMode.HALF_UP);
            if (needDeduct.compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            GiftCardCashCredential credential = credentialMapper.selectById(item.getCredentialId());
            if (credential == null || Boolean.TRUE.equals(credential.getDeleteFlag())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡不存在或已失效");
            }
            if ("PICKUP".equals(credential.getCardType())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "提货卡不支持现金抵扣");
            }
            if (!Objects.equals(memberId, credential.getActivateMemberId())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡归属校验失败");
            }
            if (!GiftCardCashCredentialStatusEnum.BOUND.name().equals(credential.getStatus())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡状态不可用");
            }
            if (credential.getExpireTime() != null && !credential.getExpireTime().after(now)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡已过期");
            }
            BigDecimal balance = credential.getBalance() == null
                    ? zeroMoney() : credential.getBalance().setScale(2, RoundingMode.HALF_UP);
            if (balance.compareTo(needDeduct) < 0) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡余额不足，请刷新后重试");
            }
            BigDecimal newBalance = balance.subtract(needDeduct).setScale(2, RoundingMode.HALF_UP);
            int updated = credentialMapper.update(null, new LambdaUpdateWrapper<GiftCardCashCredential>()
                    .set(GiftCardCashCredential::getBalance, newBalance)
                    .eq(GiftCardCashCredential::getId, credential.getId())
                    .eq(GiftCardCashCredential::getDeleteFlag, false)
                    .eq(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.BOUND.name())
                    .eq(GiftCardCashCredential::getActivateMemberId, memberId)
                    .eq(GiftCardCashCredential::getBalance, balance));
            if (updated != 1) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡扣减失败，请稍后重试");
            }
        }
    }

    /**
     * 下单时应用礼品卡分摊与扣减流水记录。
     *
     * @param credentialMapper 卡密凭证映射
     * @param orderGiftCardRecordMapper 礼品卡订单流水映射
     * @param tradeDTO 交易传输对象
     * @param orders 拆分后的主订单列表
     * @param orderItems 订单子项列表
     */
    public static void applyGiftCardUsageOnOrderCreate(GiftCardCashCredentialMapper credentialMapper,
                                                       OrderGiftCardRecordMapper orderGiftCardRecordMapper,
                                                       TradeDTO tradeDTO,
                                                       List<Order> orders,
                                                       List<OrderItem> orderItems) {
        if (tradeDTO == null || CollUtil.isEmpty(orders)) {
            return;
        }
        Map<String, BigDecimal> orderDeductMap = applyGiftCardAllocation(orders, orderItems, tradeDTO);
        List<OrderGiftCardRecord> records = buildOrderGiftCardRecords(credentialMapper, tradeDTO, orders, orderDeductMap);
        if (CollUtil.isNotEmpty(records)) {
            records.forEach(orderGiftCardRecordMapper::insert);
        }
        deductMemberCardBalance(credentialMapper, tradeDTO.getMemberId(), tradeDTO.getGiftCardDeductItems());
    }

    /**
     * 售后退还礼品卡余额（按售后实退金额全额尝试退回）。
     */
    public static BigDecimal refundMemberCardBalance(OrderGiftCardRecordMapper orderGiftCardRecordMapper,
                                                     GiftCardCashCredentialMapper credentialMapper,
                                                     AfterSale afterSale) {
        BigDecimal refundLimit = afterSale == null || afterSale.getActualRefundPrice() == null
                ? zeroMoney() : BigDecimal.valueOf(afterSale.getActualRefundPrice()).setScale(2, RoundingMode.HALF_UP);
        return refundMemberCardBalance(orderGiftCardRecordMapper, credentialMapper, afterSale, refundLimit);
    }

    /**
     * 售后退还礼品卡余额（指定回卡上限）。
     */
    public static BigDecimal refundMemberCardBalance(OrderGiftCardRecordMapper orderGiftCardRecordMapper,
                                                     GiftCardCashCredentialMapper credentialMapper,
                                                     AfterSale afterSale,
                                                     BigDecimal refundLimit) {
        if (afterSale == null || CharSequenceUtil.isBlank(afterSale.getOrderSn())) {
            return zeroMoney();
        }
        return refundOrderMemberCardBalance(orderGiftCardRecordMapper, credentialMapper, afterSale.getOrderSn(), refundLimit);
    }

    /**
     * 按订单编号退还礼品卡余额。
     */
    public static BigDecimal refundOrderMemberCardBalance(OrderGiftCardRecordMapper orderGiftCardRecordMapper,
                                                          GiftCardCashCredentialMapper credentialMapper,
                                                          String orderSn,
                                                          BigDecimal refundLimit) {
        return refundOrderMemberCardBalance(orderGiftCardRecordMapper, credentialMapper, orderSn, refundLimit, "ORDER_REFUND");
    }

    /**
     * 按订单编号退还礼品卡余额（支持自定义业务类型）。
     */
    public static BigDecimal refundOrderMemberCardBalance(OrderGiftCardRecordMapper orderGiftCardRecordMapper,
                                                          GiftCardCashCredentialMapper credentialMapper,
                                                          String orderSn,
                                                          BigDecimal refundLimit,
                                                          String bizType) {
        if (CharSequenceUtil.isBlank(orderSn)) {
            return zeroMoney();
        }
        List<OrderGiftCardRecord> records = listOrderGiftCardRecords(orderGiftCardRecordMapper, orderSn);
        if (CollUtil.isEmpty(records)) {
            return zeroMoney();
        }
        BigDecimal orderGiftCardTotal = records.stream()
                .filter(record -> "DECREASE".equals(record.getFlowType()))
                .map(OrderGiftCardRecord::getDeductAmount)
                .filter(Objects::nonNull)
                .map(v -> v.setScale(2, RoundingMode.HALF_UP))
                .reduce(zeroMoney(), BigDecimal::add);
        BigDecimal refundedTotal = records.stream()
                .filter(record -> "INCREASE".equals(record.getFlowType()))
                .map(OrderGiftCardRecord::getChangeAmount)
                .filter(Objects::nonNull)
                .map(v -> v.setScale(2, RoundingMode.HALF_UP))
                .reduce(zeroMoney(), BigDecimal::add);
        BigDecimal refundableTotal = orderGiftCardTotal.subtract(refundedTotal).setScale(2, RoundingMode.HALF_UP);
        if (refundableTotal.compareTo(BigDecimal.ZERO) <= 0) {
            return zeroMoney();
        }
        BigDecimal refundRequest = refundLimit == null ? refundableTotal : refundLimit.setScale(2, RoundingMode.HALF_UP);
        if (refundRequest.compareTo(BigDecimal.ZERO) <= 0) {
            return zeroMoney();
        }
        BigDecimal shouldRefundToCard = refundRequest.min(refundableTotal).setScale(2, RoundingMode.HALF_UP);
        if (shouldRefundToCard.compareTo(BigDecimal.ZERO) <= 0) {
            return zeroMoney();
        }
        List<OrderGiftCardRecord> deductRecords = records.stream()
                .filter(record -> "DECREASE".equals(record.getFlowType()))
                .filter(record -> record.getDeductAmount() != null && record.getDeductAmount().compareTo(BigDecimal.ZERO) > 0)
                .collect(Collectors.toList());
        if (CollUtil.isEmpty(deductRecords)) {
            return zeroMoney();
        }
        BigDecimal refundedAssigned = zeroMoney();
        BigDecimal actualRefundedToCard = zeroMoney();
        Date now = new Date();
        for (int i = 0; i < deductRecords.size(); i++) {
            OrderGiftCardRecord record = deductRecords.get(i);
            BigDecimal recordDeduct = record.getDeductAmount().setScale(2, RoundingMode.HALF_UP);
            BigDecimal currentRefund;
            if (i == deductRecords.size() - 1) {
                currentRefund = shouldRefundToCard.subtract(refundedAssigned).setScale(2, RoundingMode.HALF_UP);
            } else {
                BigDecimal ratio = recordDeduct.divide(orderGiftCardTotal, 10, RoundingMode.HALF_UP);
                currentRefund = shouldRefundToCard.multiply(ratio).setScale(2, RoundingMode.HALF_UP);
                refundedAssigned = refundedAssigned.add(currentRefund);
            }
            if (currentRefund.compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            GiftCardCashCredential credential = credentialMapper.selectById(record.getCredentialId());
            if (credential == null || Boolean.TRUE.equals(credential.getDeleteFlag())) {
                continue;
            }
            BigDecimal oldBalance = credential.getBalance() == null
                    ? zeroMoney() : credential.getBalance().setScale(2, RoundingMode.HALF_UP);
            BigDecimal faceValue = credential.getFaceValue() == null
                    ? zeroMoney() : credential.getFaceValue().setScale(2, RoundingMode.HALF_UP);
            BigDecimal targetBalance = oldBalance.add(currentRefund).setScale(2, RoundingMode.HALF_UP);
            BigDecimal newBalance = targetBalance.min(faceValue);
            if (newBalance.compareTo(oldBalance) <= 0) {
                continue;
            }
            LambdaUpdateWrapper<GiftCardCashCredential> updateWrapper = new LambdaUpdateWrapper<GiftCardCashCredential>()
                    .set(GiftCardCashCredential::getBalance, newBalance)
                    .eq(GiftCardCashCredential::getId, credential.getId())
                    .eq(GiftCardCashCredential::getDeleteFlag, false)
                    .eq(GiftCardCashCredential::getBalance, oldBalance);
            if (credential.getExpireTime() != null && !credential.getExpireTime().after(now)) {
                updateWrapper.set(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.BOUND.name());
            }
            int updated = credentialMapper.update(null, updateWrapper);
            if (updated == 1) {
                actualRefundedToCard = actualRefundedToCard.add(newBalance.subtract(oldBalance)).setScale(2, RoundingMode.HALF_UP);
                saveGiftCardUsageRecord(orderGiftCardRecordMapper, orderSn, null, credential.getActivateMemberId(), credential,
                        newBalance.subtract(oldBalance), "INCREASE", bizType, orderSn, newBalance);
            } else {
                throw new ServiceException(ResultCode.ERROR, "礼品卡回退失败，请稍后重试");
            }
        }
        return actualRefundedToCard;
    }

    /**
     * 计算订单取消后渠道退款金额。
     */
    public static BigDecimal resolveChannelRefundAfterGiftCard(OrderGiftCardRecordMapper orderGiftCardRecordMapper,
                                                               GiftCardCashCredentialMapper credentialMapper,
                                                               String orderSn,
                                                               BigDecimal orderAmount) {
        BigDecimal channelAmount = toMoney(orderAmount);
        if (CharSequenceUtil.isBlank(orderSn)) {
            return zeroMoney();
        }
        refundOrderMemberCardBalance(orderGiftCardRecordMapper, credentialMapper, orderSn, null, "ORDER_CANCEL");
        return channelAmount.compareTo(BigDecimal.ZERO) <= 0 ? zeroMoney() : channelAmount;
    }

    /**
     * 计算售后单扣除礼品卡回退后的渠道退款金额。
     */
    public static BigDecimal resolveChannelRefundAfterGiftCardForAfterSale(OrderGiftCardRecordMapper orderGiftCardRecordMapper,
                                                                           GiftCardCashCredentialMapper credentialMapper,
                                                                           OrderItemService orderItemService,
                                                                           AfterSale afterSale) {
        if (afterSale == null) {
            return zeroMoney();
        }
        BigDecimal totalAmount = toMoney(afterSale.getActualRefundPrice());
        if (totalAmount.compareTo(BigDecimal.ZERO) <= 0) {
            return zeroMoney();
        }
        BigDecimal giftCardRefundLimit = resolveGiftCardRefundLimit(orderItemService, afterSale);
        BigDecimal giftCardRefund = refundMemberCardBalance(orderGiftCardRecordMapper, credentialMapper, afterSale, giftCardRefundLimit);
        BigDecimal channelRefund = totalAmount.subtract(giftCardRefund).setScale(2, RoundingMode.HALF_UP);
        return channelRefund.compareTo(BigDecimal.ZERO) <= 0 ? zeroMoney() : channelRefund;
    }

    /**
     * 分页查询礼品卡使用明细记录。
     */
    public static IPage<GiftCardUsageRecordVO> pageUsageRecord(OrderGiftCardRecordMapper orderGiftCardRecordMapper,
                                                               GiftCardUsageRecordSearchParams params,
                                                               PageVO page) {
        Page<GiftCardUsageRecordVO> mpPage = PageUtil.initPage(page);
        QueryWrapper<GiftCardUsageRecordVO> w = new QueryWrapper<>();
        w.eq("ogr.delete_flag", false);
        // 提货取消的零金额处理记录仅用于库存幂等，不作为买家的余额变动展示。
        w.and(q -> q.isNull("ogr.biz_type").or().ne("ogr.biz_type", "PICKUP_CANCEL"));
        if (params != null) {
            // 按凭据所属活动隔离使用记录，不能用可能重名的礼品卡名称筛选。
            if (CharSequenceUtil.isNotBlank(params.getActivityId())) {
                w.eq("c.activity_id", params.getActivityId().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getOrderSn())) {
                w.eq("ogr.order_sn", params.getOrderSn().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getTradeSn())) {
                w.eq("ogr.trade_sn", params.getTradeSn().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getMemberId())) {
                w.eq("ogr.member_id", params.getMemberId().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getMemberMobile())) {
                w.eq("m.mobile", params.getMemberMobile().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getMemberName())) {
                String memberName = params.getMemberName().trim();
                w.and(q -> q.like("m.nick_name", memberName).or().like("m.username", memberName));
            }
            if (CharSequenceUtil.isNotBlank(params.getCardNo())) {
                w.like("ogr.card_no", params.getCardNo().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getFlowType())) {
                w.eq("ogr.flow_type", params.getFlowType().trim());
            }
            String transactionType = normalizeUsageTransactionType(params);
            if (CharSequenceUtil.isNotBlank(transactionType)) {
                w.eq("ogr.biz_type", transactionType);
            }
            if (CharSequenceUtil.isNotBlank(params.getBizSn())) {
                w.eq("ogr.biz_sn", params.getBizSn().trim());
            }
            if (params.getTransactionTimeStart() != null) {
                w.ge("ogr.create_time", params.getTransactionTimeStart());
            }
            if (params.getTransactionTimeEnd() != null) {
                w.le("ogr.create_time", params.getTransactionTimeEnd());
            }
        }
        w.orderByDesc("ogr.create_time");
        return orderGiftCardRecordMapper.pageUsageRecord(mpPage, w);
    }

    /**
     * 写入礼品卡使用记录（增减通用）。
     */
    public static void saveGiftCardUsageRecord(OrderGiftCardRecordMapper orderGiftCardRecordMapper,
                                               String orderSn,
                                               String tradeSn,
                                               String memberId,
                                               GiftCardCashCredential credential,
                                               BigDecimal changeAmount,
                                               String flowType,
                                               String bizType,
                                               String bizSn,
                                               BigDecimal balanceAfter) {
        if (credential == null || changeAmount == null || changeAmount.compareTo(BigDecimal.ZERO) == 0) {
            return;
        }
        OrderGiftCardRecord record = new OrderGiftCardRecord();
        record.setId(SnowFlake.getIdStr());
        record.setOrderSn(orderSn);
        record.setTradeSn(tradeSn);
        record.setMemberId(memberId);
        record.setCredentialId(credential.getId());
        record.setCardNo(credential.getCardNo());
        if (changeAmount.compareTo(BigDecimal.ZERO) < 0) {
            record.setDeductAmount(changeAmount.abs().setScale(2, RoundingMode.HALF_UP));
        } else {
            record.setDeductAmount(zeroMoney());
        }
        record.setChangeAmount(changeAmount.setScale(2, RoundingMode.HALF_UP));
        record.setBalanceAfter(balanceAfter == null ? null : balanceAfter.setScale(2, RoundingMode.HALF_UP));
        record.setFlowType(flowType);
        record.setBizType(bizType);
        record.setBizSn(bizSn);
        orderGiftCardRecordMapper.insert(record);
    }

    private static Map<String, BigDecimal> applyGiftCardAllocation(List<Order> orders, List<OrderItem> orderItems, TradeDTO tradeDTO) {
        Map<String, BigDecimal> orderDeductMap = new LinkedHashMap<>();
        BigDecimal totalDeduct = toMoney(tradeDTO.getGiftCardDeductTotal());
        BigDecimal totalCashPay = toMoney(tradeDTO.getCashPayTotal());
        if (totalDeduct.compareTo(BigDecimal.ZERO) <= 0 && totalCashPay.compareTo(BigDecimal.ZERO) <= 0) {
            return orderDeductMap;
        }
        BigDecimal orderAmountTotal = BigDecimal.ZERO;
        Map<String, BigDecimal> orderAmountMap = new HashMap<>();
        for (Order order : orders) {
            BigDecimal amount = amountBeforeGiftCard(order.getPriceDetailDTO(), order.getFlowPrice());
            orderAmountMap.put(order.getSn(), amount);
            orderAmountTotal = orderAmountTotal.add(amount);
        }
        if (orderAmountTotal.compareTo(BigDecimal.ZERO) <= 0) {
            return orderDeductMap;
        }
        BigDecimal deductedAssigned = BigDecimal.ZERO;
        BigDecimal cashAssigned = BigDecimal.ZERO;
        for (int i = 0; i < orders.size(); i++) {
            Order order = orders.get(i);
            BigDecimal orderAmount = orderAmountMap.get(order.getSn());
            BigDecimal orderDeduct;
            BigDecimal orderCash;
            if (i == orders.size() - 1) {
                orderDeduct = totalDeduct.subtract(deductedAssigned).setScale(2, RoundingMode.HALF_UP);
                orderCash = totalCashPay.subtract(cashAssigned).setScale(2, RoundingMode.HALF_UP);
            } else {
                BigDecimal ratio = orderAmount.divide(orderAmountTotal, 10, RoundingMode.HALF_UP);
                orderDeduct = totalDeduct.multiply(ratio).setScale(2, RoundingMode.HALF_UP);
                orderCash = totalCashPay.multiply(ratio).setScale(2, RoundingMode.HALF_UP);
                deductedAssigned = deductedAssigned.add(orderDeduct);
                cashAssigned = cashAssigned.add(orderCash);
            }
            PriceDetailDTO orderDetail = order.getPriceDetailDTO();
            if (orderDetail == null) {
                orderDetail = new PriceDetailDTO();
            }
            orderDetail.setGiftCardPrice(orderDeduct.doubleValue());
            order.setPriceDetailDTO(orderDetail);
            if (orderDeduct.compareTo(BigDecimal.ZERO) > 0) {
                order.setDeductionMethod("GIFT_CARD");
            }
            orderDeductMap.put(order.getSn(), orderDeduct);
            applyGiftCardAllocationToOrderItems(orderItems, order.getSn(), orderDeduct, orderCash, orderAmount);
        }
        return orderDeductMap;
    }

    private static void applyGiftCardAllocationToOrderItems(List<OrderItem> orderItems, String orderSn, BigDecimal orderDeduct,
                                                            BigDecimal orderCash, BigDecimal orderAmount) {
        List<OrderItem> currentItems = orderItems.stream()
                .filter(item -> Objects.equals(orderSn, item.getOrderSn()))
                .collect(Collectors.toList());
        if (currentItems.isEmpty()) {
            return;
        }
        if (orderAmount.compareTo(BigDecimal.ZERO) <= 0) {
            for (OrderItem item : currentItems) {
                PriceDetailDTO detailDTO = item.getPriceDetailDTO();
                if (detailDTO == null) {
                    detailDTO = new PriceDetailDTO();
                }
                detailDTO.setGiftCardPrice(0D);
                item.setPriceDetailDTO(detailDTO);
            }
            return;
        }
        BigDecimal deductedAssigned = BigDecimal.ZERO;
        BigDecimal cashAssigned = BigDecimal.ZERO;
        for (int i = 0; i < currentItems.size(); i++) {
            OrderItem item = currentItems.get(i);
            BigDecimal itemAmount = amountBeforeGiftCard(item.getPriceDetailDTO(), item.getFlowPrice());
            BigDecimal itemDeduct;
            BigDecimal itemCash;
            if (i == currentItems.size() - 1) {
                itemDeduct = orderDeduct.subtract(deductedAssigned).setScale(2, RoundingMode.HALF_UP);
                itemCash = orderCash.subtract(cashAssigned).setScale(2, RoundingMode.HALF_UP);
            } else {
                BigDecimal ratio = itemAmount.divide(orderAmount, 10, RoundingMode.HALF_UP);
                itemDeduct = orderDeduct.multiply(ratio).setScale(2, RoundingMode.HALF_UP);
                itemCash = orderCash.multiply(ratio).setScale(2, RoundingMode.HALF_UP);
                deductedAssigned = deductedAssigned.add(itemDeduct);
                cashAssigned = cashAssigned.add(itemCash);
            }
            PriceDetailDTO detailDTO = item.getPriceDetailDTO();
            if (detailDTO == null) {
                detailDTO = new PriceDetailDTO();
            }
            detailDTO.setGiftCardPrice(itemDeduct.doubleValue());
            item.setPriceDetailDTO(detailDTO);
        }
    }

    /**
     * 礼品卡已经参与价格重算，flowPrice可能是抵扣后的现金应付。
     * 分摊使用记录时需要用“现金应付 + 礼品卡抵扣”还原本次可分摊金额。
     */
    private static BigDecimal amountBeforeGiftCard(PriceDetailDTO detailDTO, Double fallbackFlowPrice) {
        BigDecimal flowPrice = toMoney(fallbackFlowPrice);
        if (detailDTO == null) {
            return flowPrice;
        }
        BigDecimal giftCardPrice = toMoney(detailDTO.getGiftCardPrice());
        return flowPrice.add(giftCardPrice).setScale(2, RoundingMode.HALF_UP);
    }

    private static List<OrderGiftCardRecord> buildOrderGiftCardRecords(GiftCardCashCredentialMapper credentialMapper,
                                                                      TradeDTO tradeDTO,
                                                                      List<Order> orders,
                                                                      Map<String, BigDecimal> orderDeductMap) {
        if (tradeDTO.getGiftCardDeductItems() == null || tradeDTO.getGiftCardDeductItems().isEmpty()) {
            return Collections.emptyList();
        }
        if (orders == null || orders.isEmpty()) {
            return Collections.emptyList();
        }
        List<OrderGiftCardRecord> records = new ArrayList<>();
        List<Order> deductibleOrders = orders.stream()
                .filter(order -> toMoney(orderDeductMap.get(order.getSn())).compareTo(BigDecimal.ZERO) > 0)
                .collect(Collectors.toList());
        if (deductibleOrders.isEmpty()) {
            return records;
        }
        BigDecimal totalOrderDeduct = deductibleOrders.stream()
                .map(order -> toMoney(orderDeductMap.get(order.getSn())))
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        if (totalOrderDeduct.compareTo(BigDecimal.ZERO) <= 0) {
            return records;
        }
        for (GiftCardCashDeductPreviewVO.DeductItem item : tradeDTO.getGiftCardDeductItems()) {
            if (item == null || item.getDeductAmount() == null || item.getDeductAmount().compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            GiftCardCashCredential credential = credentialMapper.selectById(item.getCredentialId());
            if (credential == null || Boolean.TRUE.equals(credential.getDeleteFlag())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡不存在或已失效");
            }
            BigDecimal runningBalance = credential.getBalance() == null
                    ? zeroMoney() : credential.getBalance().setScale(2, RoundingMode.HALF_UP);
            BigDecimal cardDeductTotal = item.getDeductAmount().setScale(2, RoundingMode.HALF_UP);
            BigDecimal assigned = BigDecimal.ZERO;
            for (int i = 0; i < deductibleOrders.size(); i++) {
                Order order = deductibleOrders.get(i);
                BigDecimal orderDeduct = toMoney(orderDeductMap.get(order.getSn()));
                BigDecimal cardOrderDeduct;
                if (i == deductibleOrders.size() - 1) {
                    cardOrderDeduct = cardDeductTotal.subtract(assigned).setScale(2, RoundingMode.HALF_UP);
                } else {
                    BigDecimal ratio = orderDeduct.divide(totalOrderDeduct, 10, RoundingMode.HALF_UP);
                    cardOrderDeduct = cardDeductTotal.multiply(ratio).setScale(2, RoundingMode.HALF_UP);
                    assigned = assigned.add(cardOrderDeduct);
                }
                if (cardOrderDeduct.compareTo(BigDecimal.ZERO) <= 0) {
                    continue;
                }
                OrderGiftCardRecord record = new OrderGiftCardRecord();
                record.setId(SnowFlake.getIdStr());
                record.setOrderSn(order.getSn());
                record.setTradeSn(tradeDTO.getSn());
                record.setMemberId(tradeDTO.getMemberId());
                record.setCredentialId(item.getCredentialId());
                record.setCardNo(item.getCardNo());
                record.setDeductAmount(cardOrderDeduct);
                record.setChangeAmount(cardOrderDeduct.negate());
                runningBalance = runningBalance.subtract(cardOrderDeduct).setScale(2, RoundingMode.HALF_UP);
                record.setBalanceAfter(runningBalance);
                record.setFlowType("DECREASE");
                record.setBizType("ORDER_DEDUCT");
                record.setBizSn(order.getSn());
                records.add(record);
            }
        }
        return records;
    }

    private static BigDecimal resolveGiftCardRefundLimit(OrderItemService orderItemService, AfterSale afterSale) {
        if (afterSale == null || afterSale.getActualRefundPrice() == null) {
            return zeroMoney();
        }
        double actualRefund = afterSale.getActualRefundPrice();
        if (actualRefund <= 0D) {
            return zeroMoney();
        }
        OrderItem orderItem = orderItemService.getBySn(afterSale.getOrderItemSn());
        if (orderItem == null || orderItem.getNum() == null || orderItem.getNum() <= 0) {
            return toMoney(actualRefund);
        }
        PriceDetailDTO detail = orderItem.getPriceDetailDTO();
        double itemGiftCard = detail == null || detail.getGiftCardPrice() == null ? 0D : detail.getGiftCardPrice();
        if (itemGiftCard <= 0D) {
            return zeroMoney();
        }
        double unitGiftCard = itemGiftCard / orderItem.getNum();
        double refundItemGiftCard = unitGiftCard * afterSale.getNum();
        BigDecimal upper = toMoney(refundItemGiftCard);
        BigDecimal actual = toMoney(actualRefund);
        if (upper.compareTo(BigDecimal.ZERO) < 0) {
            upper = zeroMoney();
        }
        return upper.min(actual);
    }

    private static List<OrderGiftCardRecord> listOrderGiftCardRecords(OrderGiftCardRecordMapper orderGiftCardRecordMapper, String orderSn) {
        return orderGiftCardRecordMapper.selectList(new LambdaQueryWrapper<OrderGiftCardRecord>()
                .eq(OrderGiftCardRecord::getOrderSn, orderSn)
                .eq(OrderGiftCardRecord::getDeleteFlag, false)
                .orderByAsc(OrderGiftCardRecord::getCreateTime)
                .orderByAsc(OrderGiftCardRecord::getId));
    }

    private static String normalizeUsageTransactionType(GiftCardUsageRecordSearchParams params) {
        if (params == null) {
            return null;
        }
        String type = CharSequenceUtil.isNotBlank(params.getTransactionType())
                ? params.getTransactionType().trim()
                : CharSequenceUtil.trim(params.getBizType());
        if (CharSequenceUtil.isBlank(type)) {
            return null;
        }
        if ("ORDER_DEDUCT".equalsIgnoreCase(type)) {
            return "ORDER_DEDUCT";
        }
        if ("ORDER_REFUND".equalsIgnoreCase(type)) {
            return "ORDER_REFUND";
        }
        if ("ORDER_CANCEL".equalsIgnoreCase(type)) {
            return "ORDER_CANCEL";
        }
        return type;
    }

    public static BigDecimal toMoney(BigDecimal value) {
        return value == null ? zeroMoney() : value.setScale(2, RoundingMode.HALF_UP);
    }

    public static BigDecimal toMoney(Double value) {
        return value == null ? zeroMoney() : BigDecimal.valueOf(value).setScale(2, RoundingMode.HALF_UP);
    }

    public static BigDecimal zeroMoney() {
        return BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
    }
}
