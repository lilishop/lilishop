package cn.lili.modules.statistics.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCash;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionCashSearchParams;
import cn.lili.modules.distribution.mapper.DistributionCashMapper;
import cn.lili.modules.distribution.service.DistributionCashService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.statistics.mapper.DistributionCashStatisticsMapper;
import cn.lili.modules.statistics.service.DistributionCashStatisticsService;
import cn.lili.modules.wallet.entity.dto.MemberWalletUpdateDTO;
import cn.lili.modules.wallet.entity.dto.MemberWithdrawalMessage;
import cn.lili.modules.wallet.entity.enums.DepositServiceTypeEnum;
import cn.lili.modules.wallet.entity.enums.MemberWithdrawalDestinationEnum;
import cn.lili.modules.wallet.entity.enums.WithdrawStatusEnum;
import cn.lili.modules.wallet.service.MemberWalletService;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.rocketmq.tags.MemberTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;


/**
 * 分销佣金统计层实现
 *
 * @author pikachu
 * @since 2020-03-126 18:04:56
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class DistributionCashStatisticsServiceImpl extends ServiceImpl<DistributionCashStatisticsMapper, DistributionCash>
        implements DistributionCashStatisticsService {


    @Override
    public Integer newDistributionCash() {
        QueryWrapper queryWrapper = Wrappers.query();
        queryWrapper.eq("distribution_cash_status", WithdrawStatusEnum.APPLY.name());
        return this.count(queryWrapper);
    }
}