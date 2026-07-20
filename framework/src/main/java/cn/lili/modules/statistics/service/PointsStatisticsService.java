package cn.lili.modules.statistics.service;

import cn.lili.modules.statistics.entity.vo.PointsAnalysisVO;
import cn.lili.modules.statistics.entity.vo.PointsDistributionVO;
import cn.lili.modules.statistics.entity.vo.PointsIdentityStatVO;
import cn.lili.modules.statistics.entity.vo.PointsSourceDistributionVO;

import java.util.List;

/**
 * 积分分析统计业务层
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
public interface PointsStatisticsService {

    /**
     * 积分分析概览
     *
     * @return 积分概览数据
     */
    PointsAnalysisVO overview();

    /**
     * 客户可用积分区间分布
     *
     * @return 积分分布列表
     */
    List<PointsDistributionVO> distribution();

    /**
     * 积分累计分发来源分布
     *
     * @return 来源分布列表
     */
    List<PointsSourceDistributionVO> sourceDistribution();

    /**
     * 按客户身份的积分累计统计
     *
     * @return 身份统计列表
     */
    List<PointsIdentityStatVO> identityStat();
}
