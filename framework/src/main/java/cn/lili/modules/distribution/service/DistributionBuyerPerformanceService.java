package cn.lili.modules.distribution.service;

import cn.lili.modules.distribution.entity.dto.DistributionBuyerPerformanceQueryDTO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerPerformanceVO;

/**
 * 买家端业绩统计
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public interface DistributionBuyerPerformanceService {

    /**
     * 查询分销员业绩统计。
     *
     * @param distributionId 分销员ID
     * @param query          查询参数
     * @return 业绩统计
     */
    DistributionBuyerPerformanceVO getPerformance(String distributionId, DistributionBuyerPerformanceQueryDTO query);
}
