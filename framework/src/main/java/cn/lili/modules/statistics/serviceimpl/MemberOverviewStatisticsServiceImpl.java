package cn.lili.modules.statistics.serviceimpl;

import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.MemberAnalysisVO;
import cn.lili.modules.statistics.entity.vo.MemberGenderDistributionVO;
import cn.lili.modules.statistics.entity.vo.MemberNewTrendVO;
import cn.lili.modules.statistics.entity.vo.MemberOverviewVO;
import cn.lili.modules.statistics.entity.vo.MemberRegionDistributionVO;
import cn.lili.modules.statistics.entity.vo.OverViewMetricVO;
import cn.lili.modules.statistics.mapper.MemberOverviewStatisticsMapper;
import cn.lili.modules.statistics.mapper.MemberStatisticsMapper;
import cn.lili.modules.statistics.service.MemberOverviewStatisticsService;
import cn.lili.modules.statistics.service.MemberStatisticsService;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 会员概况与客户分析统计业务层实现
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Service
public class MemberOverviewStatisticsServiceImpl implements MemberOverviewStatisticsService {

    @Autowired
    private MemberOverviewStatisticsMapper memberOverviewStatisticsMapper;
    @Autowired
    private MemberStatisticsService memberStatisticsService;
    @Autowired
    private MemberStatisticsMapper memberStatisticsMapper;

    @Override
    public MemberOverviewVO overview(StatisticsQueryParam param) {
        Date[] dates = StatisticsDateUtil.getDateArray(param);
        Date[] previous = StatisticsDateUtil.getPreviousDateArray(dates);
        PeriodMetrics cur = calc(dates, param.getStoreId());
        PeriodMetrics last = calc(previous, param.getStoreId());

        MemberOverviewVO vo = new MemberOverviewVO();
        vo.setTotalMemberNum(OverViewMetricVO.of(cur.totalMemberNum, last.totalMemberNum));
        vo.setNewMemberNum(OverViewMetricVO.of(cur.newMemberNum, last.newMemberNum));
        vo.setPayMemberNum(OverViewMetricVO.of(cur.payMemberNum, last.payMemberNum));
        vo.setRechargeMemberNum(OverViewMetricVO.of(cur.rechargeMemberNum, last.rechargeMemberNum));
        vo.setPayAmount(OverViewMetricVO.of(cur.payAmount, last.payAmount));
        vo.setPayOrderNum(OverViewMetricVO.of(cur.payOrderNum, last.payOrderNum));
        vo.setCustomerPrice(OverViewMetricVO.of(cur.customerPrice, last.customerPrice));
        return vo;
    }

    private PeriodMetrics calc(Date[] dates, String storeId) {
        PeriodMetrics m = new PeriodMetrics();
        QueryWrapper totalQw = Wrappers.query();
        totalQw.eq("disabled", true);
        totalQw.le("create_time", dates[1]);
        m.totalMemberNum = nullToZero(memberOverviewStatisticsMapper.countMember(totalQw));

        QueryWrapper newQw = Wrappers.query();
        newQw.eq("disabled", true);
        newQw.between("create_time", dates[0], dates[1]);
        m.newMemberNum = nullToZero(memberOverviewStatisticsMapper.countMember(newQw));

        QueryWrapper orderQw = Wrappers.query();
        orderQw.eq("pay_status", PayStatusEnum.PAID.name());
        orderQw.between("payment_time", dates[0], dates[1]);
        orderQw.eq(StringUtils.isNotEmpty(storeId), "store_id", storeId);
        m.payMemberNum = nullToZero(memberOverviewStatisticsMapper.countPayMember(orderQw));
        m.payOrderNum = nullToZero(memberOverviewStatisticsMapper.countPayOrder(orderQw));
        m.payAmount = round2(nullToZero(memberOverviewStatisticsMapper.sumPayAmount(orderQw)));

        QueryWrapper rechargeQw = Wrappers.query();
        rechargeQw.eq("pay_status", PayStatusEnum.PAID.name());
        rechargeQw.between("pay_time", dates[0], dates[1]);
        m.rechargeMemberNum = nullToZero(memberOverviewStatisticsMapper.countRechargeMember(rechargeQw));

        if (m.payMemberNum > 0) {
            m.customerPrice = round2(CurrencyUtil.div(m.payAmount, m.payMemberNum));
        } else {
            m.customerPrice = 0D;
        }
        return m;
    }

    @Override
    public List<MemberNewTrendVO> newMemberTrend(StatisticsQueryParam param) {
        Date[] dates = StatisticsDateUtil.getDateArray(param);
        QueryWrapper memberQw = Wrappers.query();
        memberQw.between("create_time", dates[0], dates[1]);
        memberQw.eq("disabled", true);
        memberQw.groupBy("DATE_FORMAT(create_time,'%Y-%m-%d')");
        List<Map<String, Object>> memberGroup = memberStatisticsMapper.groupNewMemberByDay(memberQw);
        Map<String, Long> memberMap = new HashMap<>();
        if (memberGroup != null) {
            for (Map<String, Object> row : memberGroup) {
                if (row.get("day") != null) {
                    memberMap.put(row.get("day").toString(),
                            row.get("num") == null ? 0L : Long.parseLong(row.get("num").toString()));
                }
            }
        }
        List<MemberNewTrendVO> result = new ArrayList<>();
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(dates[0]);
        calendar.set(Calendar.HOUR_OF_DAY, 0);
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        while (!calendar.getTime().after(dates[1])) {
            String key = cn.hutool.core.date.DateUtil.formatDate(calendar.getTime());
            MemberNewTrendVO vo = new MemberNewTrendVO();
            vo.setDate(calendar.getTime());
            vo.setNewlyAdded(memberMap.getOrDefault(key, 0L));
            result.add(vo);
            calendar.add(Calendar.DAY_OF_MONTH, 1);
        }
        return result;
    }

    @Override
    public MemberAnalysisVO analysis(StatisticsQueryParam param) {
        Date[] dates = StatisticsDateUtil.getDateArray(param);
        MemberAnalysisVO vo = new MemberAnalysisVO();
        vo.setActiveMemberNum(memberStatisticsService.activeQuantity(dates[0]));

        QueryWrapper orderQw = Wrappers.query();
        orderQw.eq("pay_status", PayStatusEnum.PAID.name());
        orderQw.between("payment_time", dates[0], dates[1]);
        orderQw.eq(StringUtils.isNotEmpty(param.getStoreId()), "store_id", param.getStoreId());
        // subquery uses same segment without group - rebuild simpler
        QueryWrapper payQw = Wrappers.query();
        payQw.eq("pay_status", PayStatusEnum.PAID.name());
        payQw.between("payment_time", dates[0], dates[1]);
        payQw.eq(StringUtils.isNotEmpty(param.getStoreId()), "store_id", param.getStoreId());
        Map<String, Object> stats = memberOverviewStatisticsMapper.repurchaseStats(payQw);
        long payMembers = stats == null || stats.get("pay_members") == null ? 0L : Long.parseLong(stats.get("pay_members").toString());
        long repurchase = stats == null || stats.get("repurchase_members") == null ? 0L : Long.parseLong(stats.get("repurchase_members").toString());
        if (payMembers > 0) {
            vo.setRepurchaseRate(round2(CurrencyUtil.mul(CurrencyUtil.div(repurchase, payMembers), 100)));
        }

        long newMembers = memberStatisticsService.newlyAdded(dates[0], dates[1]);
        if (payMembers > 0) {
            // 近似：周期内新增且有支付视为新客占比基数用支付会员
            double newRatio = round2(CurrencyUtil.mul(CurrencyUtil.div(Math.min(newMembers, payMembers), payMembers), 100));
            vo.setNewCustomerRatio(newRatio);
            vo.setOldCustomerRatio(round2(CurrencyUtil.sub(100D, newRatio)));
        }
        return vo;
    }

    @Override
    public List<MemberGenderDistributionVO> genderDistribution() {
        QueryWrapper qw = Wrappers.query();
        qw.eq("disabled", true);
        qw.groupBy("sex");
        List<MemberGenderDistributionVO> list = memberOverviewStatisticsMapper.genderDistribution(qw);
        fillProportion(list);
        return list;
    }

    @Override
    public List<MemberRegionDistributionVO> regionDistribution() {
        QueryWrapper qw = Wrappers.query();
        qw.eq("disabled", true);
        qw.groupBy("IFNULL(region,'未知')");
        List<MemberRegionDistributionVO> list = memberOverviewStatisticsMapper.regionDistribution(qw);
        long total = 0L;
        if (list != null) {
            for (MemberRegionDistributionVO item : list) {
                total += item.getNum() == null ? 0L : item.getNum();
            }
            for (MemberRegionDistributionVO item : list) {
                if (total > 0) {
                    item.setProportion(round2(CurrencyUtil.mul(CurrencyUtil.div(item.getNum(), total), 100)));
                }
            }
        }
        return list;
    }

    private void fillProportion(List<MemberGenderDistributionVO> list) {
        long total = 0L;
        if (list == null) {
            return;
        }
        for (MemberGenderDistributionVO item : list) {
            total += item.getNum() == null ? 0L : item.getNum();
        }
        for (MemberGenderDistributionVO item : list) {
            if (total > 0) {
                item.setProportion(round2(CurrencyUtil.mul(CurrencyUtil.div(item.getNum(), total), 100)));
            }
        }
    }

    private Double round2(Double v) {
        if (v == null) {
            return 0D;
        }
        return Math.round(v * 100D) / 100D;
    }

    private Long nullToZero(Long v) {
        return v == null ? 0L : v;
    }

    private Double nullToZero(Double v) {
        return v == null ? 0D : v;
    }

    private static class PeriodMetrics {
        Long totalMemberNum;
        Long newMemberNum;
        Long payMemberNum;
        Long rechargeMemberNum;
        Double payAmount;
        Long payOrderNum;
        Double customerPrice;
    }
}
