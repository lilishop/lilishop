package cn.lili.modules.statistics.serviceimpl;

import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.member.entity.enums.PointSourceEnum;
import cn.lili.modules.member.entity.vo.MemberPointsStatisticsVO;
import cn.lili.modules.member.service.MemberPointsHistoryService;
import cn.lili.modules.statistics.entity.vo.PointsAnalysisVO;
import cn.lili.modules.statistics.entity.vo.PointsDistributionVO;
import cn.lili.modules.statistics.entity.vo.PointsIdentityStatVO;
import cn.lili.modules.statistics.entity.vo.PointsSourceDistributionVO;
import cn.lili.modules.statistics.mapper.PointsStatisticsMapper;
import cn.lili.modules.statistics.service.PointsStatisticsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * 积分分析统计业务层实现
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Service
public class PointsStatisticsServiceImpl implements PointsStatisticsService {

    private static final List<String> POINT_RANGES = Arrays.asList(
            "1-2000", "2000-10000", "10000-20000", "20000-50000", "50000-更多");

    @Autowired
    private MemberPointsHistoryService memberPointsHistoryService;
    @Autowired
    private PointsStatisticsMapper pointsStatisticsMapper;

    @Override
    public PointsAnalysisVO overview() {
        MemberPointsStatisticsVO stats = memberPointsHistoryService.queryMemberPointsStatistics();
        PointsAnalysisVO vo = new PointsAnalysisVO();
        long issued = stats == null || stats.getTotalPoint() == null ? 0L : stats.getTotalPoint();
        long available = stats == null || stats.getUnUsedPoint() == null ? 0L : stats.getUnUsedPoint();
        long used = Math.max(issued - available, 0L);
        vo.setTotalIssued(issued);
        vo.setAvailablePoint(available);
        vo.setUsedPoint(used);
        if (issued > 0) {
            vo.setUsedRate(Math.round(CurrencyUtil.mul(CurrencyUtil.div(used, issued), 100) * 100D) / 100D);
        }
        return vo;
    }

    @Override
    public List<PointsDistributionVO> distribution() {
        List<Map<String, Object>> rows = pointsStatisticsMapper.pointBucketDistribution();
        Map<String, Long> map = new java.util.HashMap<>();
        long total = 0L;
        if (rows != null) {
            for (Map<String, Object> row : rows) {
                if (row.get("pointRange") == null) {
                    continue;
                }
                String range = row.get("pointRange").toString();
                long num = row.get("memberNum") == null ? 0L : Long.parseLong(row.get("memberNum").toString());
                map.put(range, num);
                total += num;
            }
        }
        List<PointsDistributionVO> result = new ArrayList<>();
        for (String range : POINT_RANGES) {
            PointsDistributionVO vo = new PointsDistributionVO();
            vo.setPointRange(range);
            long num = map.getOrDefault(range, 0L);
            vo.setMemberNum(num);
            if (total > 0) {
                vo.setProportion(Math.round(CurrencyUtil.mul(CurrencyUtil.div(num, total), 100) * 100D) / 100D);
            }
            result.add(vo);
        }
        return result;
    }

    @Override
    public List<PointsSourceDistributionVO> sourceDistribution() {
        List<PointsSourceDistributionVO> list = pointsStatisticsMapper.sourceDistribution();
        long total = 0L;
        if (list != null) {
            for (PointsSourceDistributionVO item : list) {
                total += item.getPoint() == null ? 0L : item.getPoint();
            }
            for (PointsSourceDistributionVO item : list) {
                String code = item.getSource() == null ? PointSourceEnum.UNKNOWN.name() : item.getSource();
                item.setSource(code);
                try {
                    item.setSourceName(PointSourceEnum.valueOf(code).description());
                } catch (Exception e) {
                    item.setSourceName(PointSourceEnum.UNKNOWN.description());
                }
                if (total > 0) {
                    item.setProportion(Math.round(CurrencyUtil.mul(CurrencyUtil.div(item.getPoint(), total), 100) * 100D) / 100D);
                }
            }
        }
        return list == null ? new ArrayList<>() : list;
    }

    @Override
    public List<PointsIdentityStatVO> identityStat() {
        List<Map<String, Object>> rows = pointsStatisticsMapper.identityStat();
        List<PointsIdentityStatVO> result = new ArrayList<>();
        long totalAvailable = 0L;
        if (rows != null) {
            for (Map<String, Object> row : rows) {
                long available = row.get("available") == null ? 0L : Long.parseLong(row.get("available").toString());
                totalAvailable += available;
            }
            for (Map<String, Object> row : rows) {
                PointsIdentityStatVO vo = new PointsIdentityStatVO();
                vo.setIdentity(row.get("identity") == null ? "默认等级/未分组" : row.get("identity").toString());
                long issued = row.get("totalIssued") == null ? 0L : Long.parseLong(row.get("totalIssued").toString());
                long available = row.get("available") == null ? 0L : Long.parseLong(row.get("available").toString());
                long memberCount = row.get("memberCount") == null ? 0L : Long.parseLong(row.get("memberCount").toString());
                vo.setTotalIssued(issued);
                vo.setAvailable(available);
                vo.setUsedPoint(Math.max(issued - available, 0L));
                if (totalAvailable > 0) {
                    vo.setProportion(Math.round(CurrencyUtil.mul(CurrencyUtil.div(available, totalAvailable), 100) * 100D) / 100D);
                }
                if (memberCount > 0) {
                    vo.setAvgAvailable(Math.round(CurrencyUtil.div(available, memberCount) * 100D) / 100D);
                }
                result.add(vo);
            }
        }
        return result;
    }
}
