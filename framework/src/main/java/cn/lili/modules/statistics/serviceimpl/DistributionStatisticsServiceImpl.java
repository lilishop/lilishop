package cn.lili.modules.statistics.serviceimpl;

import cn.lili.modules.distribution.entity.enums.DistributionOrderStatusEnum;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.DistributionOverviewVO;
import cn.lili.modules.statistics.entity.vo.DistributionRankVO;
import cn.lili.modules.statistics.mapper.DistributionStatisticsMapper;
import cn.lili.modules.statistics.service.DistributionStatisticsService;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import cn.lili.modules.wallet.entity.enums.WithdrawStatusEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

/**
 * 分销统计业务层实现
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Service
public class DistributionStatisticsServiceImpl implements DistributionStatisticsService {

    @Autowired
    private DistributionStatisticsMapper distributionStatisticsMapper;

    @Override
    public DistributionOverviewVO overview(StatisticsQueryParam param) {
        Date[] dates = StatisticsDateUtil.getDateArray(param);
        DistributionOverviewVO vo = new DistributionOverviewVO();

        QueryWrapper allQw = Wrappers.query();
        allQw.eq("distribution_status", DistributionStatusEnum.PASS.name());
        vo.setDistributorNum(nullToZero(distributionStatisticsMapper.countDistributor(allQw)));

        QueryWrapper applyQw = Wrappers.query();
        applyQw.eq("distribution_status", DistributionStatusEnum.APPLY.name());
        vo.setApplyNum(nullToZero(distributionStatisticsMapper.countDistributor(applyQw)));

        QueryWrapper orderQw = Wrappers.query();
        orderQw.between("create_time", dates[0], dates[1]);
        vo.setDistributionOrderNum(nullToZero(distributionStatisticsMapper.countDistributionOrder(orderQw)));
        // 分销订单金额：周期内佣金合计作为订单金额代理指标
        vo.setDistributionOrderAmount(nullToZero(distributionStatisticsMapper.sumRebate(orderQw)));

        QueryWrapper settledQw = Wrappers.query();
        settledQw.between("create_time", dates[0], dates[1]);
        settledQw.eq("distribution_order_status", DistributionOrderStatusEnum.COMPLETE.name());
        vo.setSettledCommission(nullToZero(distributionStatisticsMapper.sumRebate(settledQw)));

        QueryWrapper pendingQw = Wrappers.query();
        pendingQw.between("create_time", dates[0], dates[1]);
        pendingQw.eq("distribution_order_status", DistributionOrderStatusEnum.NO_COMPLETED.name());
        vo.setPendingCommission(nullToZero(distributionStatisticsMapper.sumRebate(pendingQw)));

        QueryWrapper cashQw = Wrappers.query();
        cashQw.between("create_time", dates[0], dates[1]);
        cashQw.in("distribution_cash_status",
                WithdrawStatusEnum.VIA_AUDITING.name(),
                WithdrawStatusEnum.D_VIA_AUDITING.name(),
                WithdrawStatusEnum.SUCCESS.name());
        vo.setCashAmount(nullToZero(distributionStatisticsMapper.sumCash(cashQw)));
        return vo;
    }

    @Override
    public List<DistributionRankVO> topDistributors(StatisticsQueryParam param) {
        Date[] dates = StatisticsDateUtil.getDateArray(param);
        QueryWrapper qw = Wrappers.query();
        qw.between("create_time", dates[0], dates[1]);
        qw.groupBy("distribution_id");
        qw.last("ORDER BY amount DESC LIMIT 10");
        List<DistributionRankVO> list = distributionStatisticsMapper.rankDistributor(qw);
        fillRank(list);
        return list;
    }

    @Override
    public List<DistributionRankVO> topGoods(StatisticsQueryParam param) {
        Date[] dates = StatisticsDateUtil.getDateArray(param);
        QueryWrapper qw = Wrappers.query();
        qw.between("create_time", dates[0], dates[1]);
        qw.groupBy("sku_id");
        qw.last("ORDER BY amount DESC LIMIT 10");
        List<DistributionRankVO> list = distributionStatisticsMapper.rankGoods(qw);
        fillRank(list);
        return list;
    }

    private void fillRank(List<DistributionRankVO> list) {
        if (list == null) {
            return;
        }
        int i = 1;
        for (DistributionRankVO vo : list) {
            vo.setRank(i++);
        }
    }

    private Long nullToZero(Long v) {
        return v == null ? 0L : v;
    }

    private Double nullToZero(Double v) {
        return v == null ? 0D : v;
    }
}
