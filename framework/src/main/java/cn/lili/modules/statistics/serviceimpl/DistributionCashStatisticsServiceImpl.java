package cn.lili.modules.statistics.serviceimpl;

import cn.lili.modules.distribution.entity.dos.DistributionCash;
import cn.lili.modules.statistics.mapper.DistributionCashStatisticsMapper;
import cn.lili.modules.statistics.service.DistributionCashStatisticsService;
import cn.lili.modules.wallet.entity.enums.WithdrawStatusEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


/**
 * 分销佣金统计层实现
 *
 * @author pikachu
 * @since 2020-03-126 18:04:56
 */
@Service
public class DistributionCashStatisticsServiceImpl extends ServiceImpl<DistributionCashStatisticsMapper, DistributionCash>
        implements DistributionCashStatisticsService {


    @Override
    public long newDistributionCash() {
        QueryWrapper queryWrapper = Wrappers.query();
        queryWrapper.eq("distribution_cash_status", WithdrawStatusEnum.APPLY.name());
        return this.count(queryWrapper);
    }
}