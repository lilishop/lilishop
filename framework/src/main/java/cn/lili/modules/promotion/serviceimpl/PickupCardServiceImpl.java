package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.goods.support.GoodsStockAggregateSupport;
import cn.lili.modules.member.entity.dos.MemberAddress;
import cn.lili.modules.member.service.MemberAddressService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.render.impl.CartPriceRender;
import cn.lili.modules.order.cart.render.impl.CartSnRender;
import cn.lili.modules.order.cart.render.impl.CheckDataRender;
import cn.lili.modules.order.cart.render.impl.SkuFreightRender;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderGiftCardRecord;
import cn.lili.modules.order.order.entity.dos.Trade;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.mapper.OrderGiftCardRecordMapper;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.service.TradeService;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.promotion.entity.dos.GiftCardCashActivity;
import cn.lili.modules.promotion.entity.dos.GiftCardCashCredential;
import cn.lili.modules.promotion.entity.dto.GiftCardCashActivityDTO;
import cn.lili.modules.promotion.entity.dto.PickupCardRedeemDTO;
import cn.lili.modules.promotion.entity.vos.PickupCardDetailVO;
import cn.lili.modules.promotion.entity.vos.PickupCardSkuVO;
import cn.lili.modules.promotion.mapper.GiftCardCashActivityMapper;
import cn.lili.modules.promotion.mapper.GiftCardCashCredentialMapper;
import cn.lili.modules.promotion.service.PickupCardRuleService;
import cn.lili.modules.promotion.service.PickupCardService;
import com.alibaba.fastjson2.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.*;

/**
 * 提货卡一次性兑换：行锁串行化同卡请求，成功核销与跨店创单同事务。
 * 商品销售价按下单时快照全额结算，免邮且不执行促销、会员价、赠品、积分或分销。
 * Redis 使用既有库存脚本原子扣减，数据库回滚时补回库存；取消仅恢复商品库存，不恢复卡权益。
 * 库存汇总复用独立组件，交易、订单及下单校验保持单向依赖，无需延迟注入。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
@Service
public class PickupCardServiceImpl implements PickupCardService {
    @Autowired
    private PickupCardRuleService pickupCardRuleService;
    @Autowired
    private GiftCardCashActivityMapper activityMapper;
    @Autowired
    private GiftCardCashCredentialMapper credentialMapper;
    @Autowired
    private GoodsSkuService goodsSkuService;
    @Autowired
    private cn.lili.cache.Cache<Object> cache;
    @Autowired
    private GoodsStockAggregateSupport stockAggregates;
    @Autowired
    private MemberAddressService memberAddressService;
    @Autowired
    private TradeService tradeService;
    @Autowired
    private OrderService orderService;
    @Autowired
    private OrderGiftCardRecordMapper recordMapper;
    @Autowired
    private CheckDataRender checkDataRender;
    @Autowired
    private SkuFreightRender freightRender;
    @Autowired
    private CartPriceRender priceRender;
    @Autowired
    private CartSnRender snRender;
    @Autowired
    private StringRedisTemplate redis;
    @Autowired
    private DefaultRedisScript<Boolean> quantityScript;

    /**
     * 复用轻量规则服务，校验与活动管理保持一致。
     */
    @Override
    public void validateActivity(GiftCardCashActivityDTO dto) {
        pickupCardRuleService.validateActivity(dto);
    }

    /**
     * 复用商品展示投影，避免向买家返回内部字段。
     */
    @Override
    public List<PickupCardSkuVO> goods(String skuIds) {
        return pickupCardRuleService.goods(skuIds);
    }

    /**
     * 查询卡归属及固定兑换规则，不返回卡密。
     */
    @Override
    public PickupCardDetailVO detail(String memberId, String credentialId) {
        GiftCardCashCredential card = ownCard(memberId, credentialId, false);
        GiftCardCashActivity activity = activityMapper.selectById(card.getActivityId());
        if (activity == null || Boolean.TRUE.equals(activity.getDeleteFlag())) throw error("提货卡活动不存在");
        PickupCardDetailVO vo = new PickupCardDetailVO();
        vo.setCredentialId(card.getId());
        vo.setGiftCardName(activity.getGiftCardName());
        vo.setPickupMode(activity.getPickupMode());
        vo.setPickupCount(activity.getPickupCount());
        vo.setGoods(goods(activity.getPickupSkuIds()));
        vo.setTradeSn(card.getPickupTradeSn());
        return vo;
    }

    /**
     * 预览始终重新查询服务端商品与地址，不修改卡或库存。
     */
    @Override
    public TradeDTO preview(String memberId, PickupCardRedeemDTO dto) {
        GiftCardCashCredential card = ownCard(memberId, dto.getCredentialId(), false);
        return buildTrade(memberId, card, dto);
    }

    /**
     * 同卡行锁串行化请求；任意步骤失败回滚，成功后重复请求返回同一交易。
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Trade redeem(String memberId, PickupCardRedeemDTO dto) {
        GiftCardCashCredential card = ownCard(memberId, dto.getCredentialId(), true);
        if (card.getPickupTradeSn() != null) return tradeService.getBySn(card.getPickupTradeSn());
        TradeDTO tradeDTO = buildTrade(memberId, card, dto);
        reserveStock(tradeDTO);
        snRender.render(tradeDTO);
        int updated = credentialMapper.update(null, new LambdaUpdateWrapper<GiftCardCashCredential>()
                .eq(GiftCardCashCredential::getId, card.getId())
                .eq(GiftCardCashCredential::getStatus, "BOUND")
                .isNull(GiftCardCashCredential::getPickupTradeSn)
                .set(GiftCardCashCredential::getStatus, "REDEEMED")
                .set(GiftCardCashCredential::getPickupTradeSn, tradeDTO.getSn()));
        if (updated != 1) throw error("提货卡已使用，请刷新查看订单");
        Trade trade = tradeService.createTrade(tradeDTO);
        List<Order> orders = orderService.list(new LambdaQueryWrapper<Order>().eq(Order::getTradeSn, trade.getSn()));
        for (Order order : orders) {
            OrderGiftCardRecord record = new OrderGiftCardRecord();
            record.setId(SnowFlake.getIdStr());
            record.setOrderSn(order.getSn());
            record.setTradeSn(trade.getSn());
            record.setMemberId(memberId);
            record.setCredentialId(card.getId());
            record.setCardNo(card.getCardNo());
            record.setDeductAmount(BigDecimal.valueOf(order.getGoodsPrice()).setScale(2, RoundingMode.HALF_UP));
            record.setChangeAmount(BigDecimal.ZERO);
            record.setBalanceAfter(BigDecimal.ZERO);
            record.setFlowType("DECREASE");
            record.setBizType("PICKUP_REDEEM");
            record.setBizSn(order.getSn());
            recordMapper.insert(record);
        }
        tradeService.payTrade(trade.getSn(), PaymentMethodEnum.NOT_ACTUALLY_PAID.name(), card.getId());
        trade.setPayStatus("PAID");
        return trade;
    }

    /**
     * 提货取消只恢复商品库存：锁定原兑换流水，追加零金额的取消处理记录保证重试幂等。
     * 取消记录与数据库库存同事务；Redis 在回滚时补偿，原兑换流水及卡权益均不删除、不恢复。
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void releaseCancelledStock(cn.lili.modules.order.order.entity.vo.OrderDetailVO detail) {
        Order order = detail.getOrder();
        if (order == null || !"CANCELLED".equals(order.getOrderStatus())) return;
        OrderGiftCardRecord redemption = recordMapper.selectOne(new LambdaQueryWrapper<OrderGiftCardRecord>()
                .eq(OrderGiftCardRecord::getOrderSn, order.getSn()).eq(OrderGiftCardRecord::getBizType, "PICKUP_REDEEM")
                .eq(OrderGiftCardRecord::getDeleteFlag, false).last("LIMIT 1 FOR UPDATE"));
        if (redemption == null) return;
        // 使用当前读，避免外层事务已建立的快照看不到上一条取消通知提交的处理记录。
        OrderGiftCardRecord cancelled = recordMapper.selectOne(new LambdaQueryWrapper<OrderGiftCardRecord>()
                .eq(OrderGiftCardRecord::getOrderSn, order.getSn()).eq(OrderGiftCardRecord::getBizType, "PICKUP_CANCEL")
                .eq(OrderGiftCardRecord::getDeleteFlag, false).last("LIMIT 1 FOR UPDATE"));
        if (cancelled != null) return;
        List<cn.lili.modules.order.order.entity.dos.OrderItem> items = detail.getOrderItems().stream()
                .sorted(Comparator.comparing(cn.lili.modules.order.order.entity.dos.OrderItem::getSkuId)).toList();
        List<String> keys = items.stream().map(i -> GoodsSkuService.getStockCacheKey(i.getSkuId())).toList();
        items.forEach(i -> goodsSkuService.getStock(i.getSkuId()));
        if (!Boolean.TRUE.equals(redis.execute(quantityScript, keys, items.stream().map(i -> String.valueOf(i.getNum())).toArray()))) {
            throw error("恢复商品库存失败，请重试");
        }
        TransactionSynchronizationManager.registerSynchronization(new StockSynchronization(
                () -> compensateStock(keys, items.stream().map(i -> String.valueOf(-i.getNum())).toArray()),
                () -> items.forEach(i -> cache.remove(GoodsSkuService.getCacheKeys(i.getSkuId())))));
        for (var item : items) {
            if (!goodsSkuService.update(new LambdaUpdateWrapper<GoodsSku>().eq(GoodsSku::getId, item.getSkuId())
                    .setSql("quantity = quantity + {0}", item.getNum()))) throw error("恢复商品库存失败，请重试");
        }
        items.stream().map(cn.lili.modules.order.order.entity.dos.OrderItem::getGoodsId).distinct().sorted().forEach(stockAggregates::updateStock);
        OrderGiftCardRecord marker = new OrderGiftCardRecord();
        marker.setId(SnowFlake.getIdStr());
        marker.setOrderSn(order.getSn());
        marker.setTradeSn(redemption.getTradeSn());
        marker.setMemberId(redemption.getMemberId());
        marker.setCredentialId(redemption.getCredentialId());
        marker.setCardNo(redemption.getCardNo());
        marker.setBizType("PICKUP_CANCEL");
        marker.setBizSn(order.getSn());
        marker.setFlowType("DECREASE");
        // 仅记录取消及库存已恢复；不扣款、不退款、不回卡，所有金额均为零。
        marker.setDeductAmount(BigDecimal.ZERO);
        marker.setChangeAmount(BigDecimal.ZERO);
        marker.setBalanceAfter(BigDecimal.ZERO);
        if (recordMapper.insert(marker) != 1) throw error("保存提货卡取消处理记录失败，请重试");
    }

    /**
     * 归属校验必须早于幂等返回，防止他人通过猜测卡 ID 获取交易。
     */
    private GiftCardCashCredential ownCard(String memberId, String id, boolean lock) {
        LambdaQueryWrapper<GiftCardCashCredential> query = new LambdaQueryWrapper<GiftCardCashCredential>()
                .eq(GiftCardCashCredential::getId, id).eq(GiftCardCashCredential::getDeleteFlag, false);
        if (lock) query.last("FOR UPDATE");
        GiftCardCashCredential card = credentialMapper.selectOne(query);
        if (card == null || !Objects.equals(memberId, card.getMemberId()) || !"PICKUP".equals(card.getCardType())) {
            throw error("提货卡不存在或不属于当前会员");
        }
        return card;
    }

    /**
     * 专用交易只走商品和配送校验，跳过所有促销，商品价格重新读取数据库销售价。
     */
    private TradeDTO buildTrade(String memberId, GiftCardCashCredential card, PickupCardRedeemDTO dto) {
        if (!"BOUND".equals(card.getStatus()) || !Objects.equals(memberId, card.getActivateMemberId())
                || card.getActivateTime() == null || card.getPickupTradeSn() != null
                || (card.getExpireTime() != null && !card.getExpireTime().after(new Date())))
            throw error("提货卡未激活、已使用或已过期");
        GiftCardCashActivity activity = activityMapper.selectById(card.getActivityId());
        if (activity == null || Boolean.TRUE.equals(activity.getDeleteFlag())) throw error("提货卡活动不存在");
        pickupCardRuleService.validateSelection(activity.getPickupMode(), activity.getPickupCount(), JSON.parseArray(activity.getPickupSkuIds(), String.class), dto.getSkuIds(), true);
        MemberAddress address = memberAddressService.getMemberAddress(dto.getAddressId());
        if (address == null || !Objects.equals(memberId, address.getMemberId()) || Boolean.TRUE.equals(address.getDeleteFlag()))
            throw error("收货地址不存在或不属于当前会员");
        TradeDTO trade = new TradeDTO(CartTypeEnum.BUY_NOW);
        trade.setClientType(resolveClientType(dto.getClient()));
        trade.setMemberId(memberId);
        trade.setMemberName(UserContext.getCurrentUser().getUsername());
        trade.setMemberAddress(address);
        trade.setPickupCardId(card.getId());
        for (String id : dto.getSkuIds()) {
            GoodsSku sku = goodsSkuService.getById(id);
            if (!pickupCardRuleService.isValidSku(sku) || goodsSkuService.getStock(id) < 1)
                throw error("所选商品已下架或库存不足，请重新选择");
            CartSkuVO item = new CartSkuVO(sku, new HashMap<>());
            item.setCartType(CartTypeEnum.BUY_NOW);
            item.setNum(1);
            trade.getSkuList().add(item);
        }
        checkDataRender.render(trade);
        if (trade.getCheckedSkuList().size() != dto.getSkuIds().size()) throw error("部分商品已失效，请刷新后重试");
        for (CartSkuVO item : trade.getSkuList()) {
            item.setPromotionMap(new HashMap<>());
            double price = BigDecimal.valueOf(item.getGoodsSku().getPrice()).setScale(2, RoundingMode.HALF_UP).doubleValue();
            item.setPurchasePrice(price);
            item.setUtilPrice(price);
            item.setSubTotal(price);
            PriceDetailDTO prices = new PriceDetailDTO();
            prices.setGoodsPrice(price);
            prices.setOriginalPrice(price);
            prices.setGiftCardPrice(price);
            // 佣金与分销均为零，商家结算基数完整保留商品销售价。
            item.setPriceDetailDTO(prices);
        }
        freightRender.render(trade);
        if (trade.getNotSupportFreight() != null && !trade.getNotSupportFreight().isEmpty())
            throw error("收货地址不在所选商品配送范围内");
        trade.getSkuList().forEach(item -> item.getPriceDetailDTO().setFreightPrice(0D));
        priceRender.render(trade);
        return trade;
    }

    /**
     * 沿用普通订单客户端枚举，兼容旧请求缺省来源，拒绝非法来源值。
     * @param client 客户端上报来源，仅作订单来源记录，不参与价格及权益判断
     * @return 订单持久化使用的客户端枚举名称
     */
    private String resolveClientType(String client) {
        if (client == null || client.isBlank()) return ClientTypeEnum.UNKNOWN.name();
        try {
            return ClientTypeEnum.valueOf(client.trim()).name();
        } catch (IllegalArgumentException e) {
            throw error("客户端来源不正确");
        }
    }

    /**
     * 与普通购买共用 Redis 库存键及原子脚本，数据库事务回滚时补偿已扣数量。
     */
    private void reserveStock(TradeDTO trade) {
        List<String> keys = trade.getSkuList().stream().map(item -> GoodsSkuService.getStockCacheKey(item.getGoodsSku().getId())).toList();
        Object[] amounts = keys.stream().map(key -> "-1").toArray();
        if (!Boolean.TRUE.equals(redis.execute(quantityScript, keys, amounts))) throw error("商品库存不足，请重新选择");
        TransactionSynchronizationManager.registerSynchronization(new StockSynchronization(
                () -> compensateStock(keys, keys.stream().map(key -> "1").toArray()),
                () -> trade.getSkuList().forEach(item -> cache.remove(GoodsSkuService.getCacheKeys(item.getGoodsSku().getId())))));
        // 固定顺序执行数据库条件扣减，不把读到的 Redis 旧值覆盖回库存缓存，也不改动促销库存。
        List<GoodsSku> skus = trade.getSkuList().stream().map(CartSkuVO::getGoodsSku).sorted(Comparator.comparing(GoodsSku::getId)).toList();
        for (GoodsSku sku : skus) {
            if (!goodsSkuService.update(new LambdaUpdateWrapper<GoodsSku>().eq(GoodsSku::getId, sku.getId())
                    .gt(GoodsSku::getQuantity, 0).setSql("quantity = quantity - 1")))
                throw error("商品库存已变更，请刷新后重试");
        }
        skus.stream().map(GoodsSku::getGoodsId).distinct().sorted().forEach(stockAggregates::updateStock);
    }

    /**
     * 数据库回滚时按本事务的反向数量补偿 Redis，失败抛异常供事务同步日志定位。
     */
    private void compensateStock(List<String> keys, Object[] quantities) {
        if (!Boolean.TRUE.equals(redis.execute(quantityScript, keys, quantities))) {
            throw new IllegalStateException("提货卡库存事务补偿失败，需要核对库存，SKU 数量=" + keys.size());
        }
    }

    /**
     * 库存事务完成回调；仅回滚时执行逆向库存变更，任何结果都清理商品信息缓存。
     * 此回调处理可正常返回的事务失败；跨进程故障恢复仍需结合库存对账。
     *
     * @author Bulbasaur
     * @since 2026-11-06
     */
    private static final class StockSynchronization implements TransactionSynchronization {
        /**
         * 数据库事务未提交时执行的库存补偿，不涉及提货卡权益。
         */
        private final Runnable rollback;
        /**
         * 无论提交还是回滚，均删除可能过时的 SKU 信息缓存。
         */
        private final Runnable clearCache;

        /**
         * 保存补偿和缓存清理动作，不在事务完成前执行。
         */
        private StockSynchronization(Runnable rollback, Runnable clearCache) {
            this.rollback = rollback;
            this.clearCache = clearCache;
        }

        /**
         * 补偿失败时仍清理缓存；补偿异常由事务同步框架记录，不静默吞掉。
         */
        @Override
        public void afterCompletion(int status) {
            try {
                if (status != STATUS_COMMITTED) rollback.run();
            } finally {
                clearCache.run();
            }
        }
    }

    /**
     * 将规则错误转换为业务异常，保持现有 API 错误响应格式。
     */
    private static ServiceException error(String message) {
        return new ServiceException(ResultCode.PARAMS_ERROR, message);
    }
}
