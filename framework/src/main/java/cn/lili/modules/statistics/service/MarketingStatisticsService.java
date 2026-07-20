package cn.lili.modules.statistics.service;

import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.MarketingOverviewVO;

/**
 * 营销概况统计业务层
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
public interface MarketingStatisticsService {

    /**
     * 营销概况指标
     *
     * @param statisticsQueryParam 统计查询参数
     * @return 营销概况
     */
    MarketingOverviewVO overview(StatisticsQueryParam statisticsQueryParam);
}
