package cn.lili.modules.statistics.service;

import cn.lili.modules.statistics.entity.vo.DepositBucketVO;

import java.util.List;

/**
 * 储值分析统计业务层
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
public interface DepositStatisticsService {

    /**
     * 储值余额区间分布
     *
     * @return 余额分布列表
     */
    List<DepositBucketVO> balanceDistribution();

    /**
     * 充值次数区间分布
     *
     * @return 充值次数分布列表
     */
    List<DepositBucketVO> rechargeTimesDistribution();

    /**
     * 充值金额区间分布
     *
     * @return 充值金额分布列表
     */
    List<DepositBucketVO> rechargeAmountDistribution();
}
