package cn.lili.modules.statistics.serviceimpl;

import cn.lili.modules.statistics.entity.vo.DepositBucketVO;
import cn.lili.modules.statistics.mapper.DepositStatisticsMapper;
import cn.lili.modules.statistics.service.DepositStatisticsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 储值分析统计业务层实现
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Service
public class DepositStatisticsServiceImpl implements DepositStatisticsService {

    private static final List<String> BALANCE_LABELS = Arrays.asList(
            "0-30", "30-60", "60-90", "90-120", "120-150", "150-180", "180-210", "210-700", "700-800", "800-更多");
    private static final List<String> TIMES_LABELS = Arrays.asList(
            "0-1", "2-3", "4-5", "6-7", "8-9", "10-11", "12-13", "14-更多");
    private static final List<String> AMOUNT_LABELS = Arrays.asList(
            "0-100", "100-300", "300-500", "500-1000", "1000-2000", "2000-5000", "5000-10000", "10000-更多");

    @Autowired
    private DepositStatisticsMapper depositStatisticsMapper;

    @Override
    public List<DepositBucketVO> balanceDistribution() {
        return fill(BALANCE_LABELS, depositStatisticsMapper.balanceDistribution());
    }

    @Override
    public List<DepositBucketVO> rechargeTimesDistribution() {
        return fill(TIMES_LABELS, depositStatisticsMapper.rechargeTimesDistribution());
    }

    @Override
    public List<DepositBucketVO> rechargeAmountDistribution() {
        return fill(AMOUNT_LABELS, depositStatisticsMapper.rechargeAmountDistribution());
    }

    private List<DepositBucketVO> fill(List<String> labels, List<Map<String, Object>> rows) {
        Map<String, Long> map = new HashMap<>();
        if (rows != null) {
            for (Map<String, Object> row : rows) {
                if (row.get("label") == null) {
                    continue;
                }
                map.put(row.get("label").toString(),
                        row.get("memberNum") == null ? 0L : Long.parseLong(row.get("memberNum").toString()));
            }
        }
        List<DepositBucketVO> result = new ArrayList<>();
        for (String label : labels) {
            DepositBucketVO vo = new DepositBucketVO();
            vo.setLabel(label);
            vo.setMemberNum(map.getOrDefault(label, 0L));
            result.add(vo);
        }
        return result;
    }
}
