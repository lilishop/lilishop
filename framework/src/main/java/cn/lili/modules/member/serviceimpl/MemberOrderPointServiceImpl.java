package cn.lili.modules.member.serviceimpl;

import cn.lili.modules.promotion.service.PickupCardOrderService;
import cn.lili.modules.member.entity.enums.PointSourceEnum;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.service.MemberOrderPointService;
import cn.lili.modules.member.mapper.MemberPointsHistoryMapper;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.member.service.MemberBenefitService;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.system.entity.dto.PointSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.hutool.json.JSONUtil;
import cn.lili.modules.order.aftersale.mapper.AfterSaleMapper;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.mapper.OrderItemMapper;
import cn.lili.modules.order.order.mapper.OrderMapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Set;
import java.util.List;

/**
 * 售后期结束且所有售后均终结后，按最终保留金额一次性结算购物积分。
 * 按单实例单任务模式执行，复用现有积分流水防重，不增加订单字段或额外行锁。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Service("memberOrderPointService")
public class MemberOrderPointServiceImpl implements MemberOrderPointService {

    @Autowired
    private PickupCardOrderService pickupCardOrderService;
    @Autowired
    private OrderMapper orders;
    @Autowired
    private OrderItemMapper items;
    @Autowired
    private AfterSaleMapper afterSales;
    @Autowired
    private MemberService memberService;
    @Autowired
    private MemberPointsHistoryMapper histories;
    @Autowired
    private SettingService settings;
    @Autowired
    private MemberBenefitService benefits;

    /**
     * 已终结的售后状态；其他状态（包括未知或空状态）均阻止积分发放。
     */
    private static final Set<String> TERMINAL = Set.of("COMPLETE", "REFUSE", "SELLER_TERMINATION", "BUYER_CANCEL");

    /**
     * 按最终保留金额计算赠送积分，退款超过原金额时按零计算。
     *
     * @param flow       原订单流水金额，单位元
     * @param refund     已完成售后实际退款合计，单位元
     * @param rate       每元赠送积分数
     * @param multiplier 发放时客户当前有效权益的最高积分倍率
     * @return 四舍五入到整数的积分数，超出 long 范围时抛出算术异常
     */
    static long remainingPoints(BigDecimal flow, BigDecimal refund, BigDecimal rate, BigDecimal multiplier) {
        return flow.subtract(refund).max(BigDecimal.ZERO).multiply(rate).multiply(multiplier)
                .setScale(0, RoundingMode.HALF_UP).longValueExact();
    }

    /**
     * 在交易完结入口按订单号结算一次购物积分，余额与积分流水在同一事务内保存。
     * 以商品项已关闭售后入口、全部售后已终结为准，不重复读取设置推算售后截止时间。
     * 按发放时当前积分设置及客户权益倍率计算；已赠送、积分兑换及最终积分为零的订单无需再次赠送。
     *
     * @param orderSn 订单编号，用于读取数据库中的最新订单及售后数据
     * @return 已处理或无需赠送返回 true；订单未完成或仍可售后时返回 false，保留待处理流水
     * @throws IllegalStateException 订单不存在、退款金额异常或积分入账失败，调用方保留流水供下次重试
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public boolean complete(String orderSn) {
        // 使用持久化订单的最终金额和状态，积分比例及倍率在实际发放时查询。
        Order order = orders.selectOne(new LambdaQueryWrapper<Order>().eq(Order::getSn, orderSn));
        if (order == null) throw new IllegalStateException("订单不存在");
        if (!OrderStatusEnum.COMPLETED.name().equals(order.getOrderStatus())) return false;
        // 提货卡及积分兑换不送购物积分；已有赠送流水说明本订单已经处理。评价奖励由独立评价事件处理。
        if (pickupCardOrderService.isPickupOrder(order.getSn()) || "POINTS".equals(order.getOrderPromotionType())
                || histories.findOrderReward(order.getMemberId(), orderSn) != null) return true;

        List<AfterSale> refunds = afterSales.memberPointAfterSales(orderSn);
        if (!isAfterSaleClosed(orderSn, refunds)) return false;
        long earned = calculatePoints(order, refunds);
        if (earned > 0) grantPoints(order, earned);
        return true;
    }

    /**
     * 确认全部商品项已关闭售后入口，且没有处理中、未知或空状态的售后。
     * 已到期但入口尚未关闭不能发放；同一商品项存在多次售后时必须全部终结。
     *
     * @param orderSn 订单编号，对应订单商品项及售后记录的 order_sn
     * @param refunds 订单全部售后记录，包含用于核对实际退款的历史记录
     * @return 商品项存在、全部不可售后且全部售后终结时返回 true
     */
    private boolean isAfterSaleClosed(String orderSn, List<AfterSale> refunds) {
        List<OrderItem> orderItems = items.selectList(new LambdaQueryWrapper<OrderItem>().eq(OrderItem::getOrderSn, orderSn));
        return !orderItems.isEmpty()
                && orderItems.stream().allMatch(item -> "EXPIRED".equals(item.getAfterSaleStatus()))
                && refunds.stream().allMatch(row -> row.getServiceStatus() != null && TERMINAL.contains(row.getServiceStatus()));
    }

    /**
     * 按发放时的当前积分设置及权益倍率计算，仅扣除已完成售后的实际退款，拒绝和取消不扣减。
     * 配置变更、等级变化和付费会员到期会影响尚未赠送的订单；退款超过原金额按零计算。
     *
     * @param order 持久化订单，流水金额单位元，客户 ID 用于查询当前权益倍率
     * @param refunds 已核验全部终结的售后记录，实际退款金额单位元
     * @return 最终保留金额乘积分比例及倍率，四舍五入到整数积分
     * @throws IllegalStateException 实际退款金额为空、非有限数或负数，或计算结果为负数
     */
    private long calculatePoints(Order order, List<AfterSale> refunds) {
        BigDecimal refunded = BigDecimal.ZERO;
        for (AfterSale row : refunds) {
            if (!"COMPLETE".equals(row.getServiceStatus())) continue;
            Double amount = row.getActualRefundPrice();
            if (amount == null || !Double.isFinite(amount) || amount < 0)
                throw new IllegalStateException("售后实际退款金额不正确");
            refunded = refunded.add(BigDecimal.valueOf(amount));
        }
        var setting = settings.get(SettingEnum.POINT_SETTING.name());
        if (setting == null) throw new IllegalStateException("积分设置不存在");
        PointSetting config = JSONUtil.toBean(setting.getSettingValue(), PointSetting.class);
        BigDecimal rate = BigDecimal.valueOf(config.getConsumer());
        if (rate.signum() == 0) return 0;
        BigDecimal multiplier = benefits.getPointMultiplier(order.getMemberId());
        long earned = remainingPoints(BigDecimal.valueOf(order.getFlowPrice()), refunded, rate, multiplier);
        if (earned < 0) throw new IllegalStateException("购物积分规则不正确");
        return earned;
    }

    /**
     * 调用现有积分服务增加余额，并确认积分切面已保存赠送流水。
     * 流水是重复任务的判断依据；更新失败或流水缺失必须抛异常，由 complete 的事务回滚余额。
     *
     * @param order 赠送所属订单，使用客户 ID 及订单编号记录流水
     * @param earned 本次实际赠送积分数，必须大于零
     * @throws IllegalStateException 积分余额更新失败或赠送流水未保存
     */
    private void grantPoints(Order order, long earned) {
        if (!Boolean.TRUE.equals(memberService.updateMemberPoint(earned, PointTypeEnum.INCREASE.name(), order.getMemberId(),
                "订单[" + order.getSn() + "]售后期结束，购物赠送积分", PointSourceEnum.ORDER.name())))
            throw new IllegalStateException("积分更新失败");
        if (histories.findOrderReward(order.getMemberId(), order.getSn()) == null)
            throw new IllegalStateException("购物积分流水保存失败");
    }
}
