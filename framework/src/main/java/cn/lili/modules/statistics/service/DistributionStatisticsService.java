package cn.lili.modules.statistics.service;

import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.DistributionOverviewVO;
import cn.lili.modules.statistics.entity.vo.DistributionRankVO;

import java.util.List;

/**
 * 分销统计业务层
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
public interface DistributionStatisticsService {

    /**
     * 分销概况指标
     *
     * @param param 统计查询参数
     * @return 分销概况
     */
    DistributionOverviewVO overview(StatisticsQueryParam param);

    /**
     * 分销员业绩排行
     *
     * @param param 统计查询参数
     * @return 排行列表
     */
    List<DistributionRankVO> topDistributors(StatisticsQueryParam param);

    /**
     * 分销商品排行
     *
     * @param param 统计查询参数
     * @return 排行列表
     */
    List<DistributionRankVO> topGoods(StatisticsQueryParam param);
}
