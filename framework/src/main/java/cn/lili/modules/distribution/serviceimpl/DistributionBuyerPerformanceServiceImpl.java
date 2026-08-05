package cn.lili.modules.distribution.serviceimpl;

import cn.lili.modules.distribution.entity.dto.DistributionBuyerPerformanceQueryDTO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerOrderSummaryVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerPerformanceVO;
import cn.lili.modules.distribution.mapper.DistributionBuyerOrderMapper;
import cn.lili.modules.distribution.mapper.DistributionBuyerPerformanceMapper;
import cn.lili.modules.distribution.mapper.DistributionCustomerRelationMapper;
import cn.lili.modules.distribution.mapper.DistributionInviteMapper;
import cn.lili.modules.distribution.service.DistributionBuyerPerformanceService;
import cn.lili.modules.distribution.support.DistributionInviteRangeSupport;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 买家端业绩统计实现
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Service
public class DistributionBuyerPerformanceServiceImpl implements DistributionBuyerPerformanceService {

    @Autowired
    private DistributionBuyerPerformanceMapper performanceMapper;

    @Autowired
    private DistributionBuyerOrderMapper buyerOrderMapper;

    @Autowired
    private DistributionCustomerRelationMapper customerRelationMapper;

    @Autowired
    private DistributionInviteMapper distributionInviteMapper;

    /**
     * 查询分销员业绩汇总（佣金、销售额、客户与邀请数）。
     *
     * @param distributionId 分销员ID
     * @param query          时间范围等查询参数
     * @return 业绩统计
     */
    @Override
    public DistributionBuyerPerformanceVO getPerformance(String distributionId,
                                                         DistributionBuyerPerformanceQueryDTO query) {
        DistributionBuyerPerformanceQueryDTO safeQuery =
                query == null ? new DistributionBuyerPerformanceQueryDTO() : query;
        DistributionInviteRangeSupport.DateRange range = DistributionInviteRangeSupport.resolveRange(
                safeQuery.getRangeType(), safeQuery.getStartTime(), safeQuery.getEndTime());
        Date start = range.getStartTime();
        Date end = range.getEndTime();

        // 一级推广佣金：累计（含待结算+已结算）与待结算部分
        BigDecimal directCommission = defaultAmount(performanceMapper.sumDirectCommission(distributionId, start, end));
        BigDecimal directPending = defaultAmount(performanceMapper.sumDirectPending(distributionId, start, end));

        // 二级邀请佣金：累计与待结算部分
        BigDecimal inviteReward = defaultAmount(performanceMapper.sumInviteCommission(distributionId, start, end));
        BigDecimal invitePending = defaultAmount(performanceMapper.sumInvitePending(distributionId, start, end));

        // 汇总收益：一级 + 二级；待结算、已结算分别相加
        BigDecimal totalEarnings = directCommission.add(inviteReward);
        BigDecimal pendingEarnings = directPending.add(invitePending);
        BigDecimal settledEarnings = defaultAmount(performanceMapper.sumDirectSettled(distributionId, start, end))
                .add(defaultAmount(performanceMapper.sumInviteSettled(distributionId, start, end)));

        DistributionBuyerPerformanceVO vo = new DistributionBuyerPerformanceVO();
        vo.setTotalEarnings(totalEarnings);
        vo.setPendingEarnings(pendingEarnings);
        vo.setSettledEarnings(settledEarnings);
        vo.setDirectCommission(directCommission);
        vo.setDirectPending(directPending);
        vo.setInviteReward(inviteReward);
        vo.setInvitePending(invitePending);

        // 推广销售额（非退款订单实付）
        vo.setTotalSales(defaultAmount(performanceMapper.sumPromotionSales(distributionId, start, end)));

        // 推广订单数、绑定客户数、邀请下级数（均受时间范围筛选）
        DistributionBuyerOrderSummaryVO orderSummary = buyerOrderMapper.summarizePromotionOrders(
                distributionId, start, end, null);
        vo.setTotalOrders(orderSummary == null ? 0L : orderSummary.getTotal());
        vo.setTotalCustomers(customerRelationMapper.countBuyerCustomers(
                distributionId, "ALL", null, start, end));
        vo.setTotalInvites(distributionInviteMapper.countInvitees(distributionId, start, end));
        return vo;
    }

    /**
     * 金额空值转为 {@link BigDecimal#ZERO}。
     *
     * @param value 原始金额
     * @return 非空金额
     */
    private BigDecimal defaultAmount(BigDecimal value) {
        return value == null ? BigDecimal.ZERO : value;
    }
}
