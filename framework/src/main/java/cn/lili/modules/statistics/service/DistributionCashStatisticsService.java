package cn.lili.modules.statistics.service;

import cn.lili.modules.distribution.entity.dos.DistributionCash;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 分销佣金统计
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
public interface DistributionCashStatisticsService extends IService<DistributionCash> {

    /**
     * 待处理分销员提现申请数量
     *
     * @return 待处理分销员提现申请数量
     */
    long newDistributionCash();
}