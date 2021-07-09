package cn.lili.modules.distribution.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.common.rocketmq.tags.MemberTagsEnum;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.vo.PageVO;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCash;
import cn.lili.modules.distribution.entity.enums.DistributionCashStatusEnum;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionCashSearchParams;
import cn.lili.modules.distribution.mapper.DistributionCashMapper;
import cn.lili.modules.distribution.service.DistributionCashService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.member.entity.dto.MemberWithdrawalMessage;
import cn.lili.modules.member.entity.enums.MemberWithdrawalDestinationEnum;
import cn.lili.modules.member.service.MemberWalletService;
import cn.lili.modules.order.trade.entity.enums.DepositServiceTypeEnum;
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
 * 分销佣金业务层实现
 *
 * @author pikachu
 * @date 2020-03-126 18:04:56
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class DistributionCashServiceImpl extends ServiceImpl<DistributionCashMapper, DistributionCash> implements DistributionCashService {
    /**
     * 分销员
     */
    @Autowired
    private DistributionService distributionService;
    /**
     * 会员余额
     */
    @Autowired
    private MemberWalletService memberWalletService;
    @Autowired
    private RocketMQTemplate rocketMQTemplate;
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Override
    public Boolean cash(Double applyMoney) {

        //检查分销功能开关
        distributionService.checkDistributionSetting();

        //获取分销员
        Distribution distribution = distributionService.getDistribution();
        //如果未找到分销员或者分销员状态不是已通过则无法申请提现
        if (distribution != null && distribution.getDistributionStatus().equals(DistributionStatusEnum.PASS.name())) {
            //校验分销佣金是否大于提现金额
            if (distribution.getCanRebate() < applyMoney) {
                throw new ServiceException(ResultCode.WALLET_WITHDRAWAL_INSUFFICIENT);
            }
            //将提现金额存入冻结金额,扣减可提现金额
            distribution.setCanRebate(CurrencyUtil.sub(distribution.getCanRebate(), applyMoney));
            distribution.setCommissionFrozen(CurrencyUtil.add(distribution.getCommissionFrozen(), applyMoney));
            distributionService.updateById(distribution);
            //提现申请记录
            DistributionCash distributionCash = new DistributionCash("D" + SnowFlake.getId(), distribution.getId(), applyMoney, distribution.getMemberName());
            Boolean result = this.save(distributionCash);
            if (result) {
                //发送提现消息
                MemberWithdrawalMessage memberWithdrawalMessage = new MemberWithdrawalMessage();
                memberWithdrawalMessage.setMemberId(distribution.getMemberId());
                memberWithdrawalMessage.setPrice(applyMoney);
                memberWithdrawalMessage.setDestination(MemberWithdrawalDestinationEnum.WALLET.name());
                String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_WITHDRAWAL.name();
                rocketMQTemplate.asyncSend(destination, memberWithdrawalMessage, RocketmqSendCallbackBuilder.commonCallback());
                return true;
            }
            return false;

        }
        throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);

    }

    @Override
    public IPage<DistributionCash> getDistributionCash(PageVO page) {
        Distribution distribution = distributionService.getDistribution();
        QueryWrapper<DistributionCash> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("distribution_id", distribution.getId());
        return this.page(PageUtil.initPage(page), queryWrapper);
    }

    @Override
    public IPage<DistributionCash> getDistributionCash(DistributionCashSearchParams distributionCashSearchParams) {

        return this.page(PageUtil.initPage(distributionCashSearchParams), distributionCashSearchParams.queryWrapper());
    }

    @Override
    public DistributionCash audit(String id, String result) {

        //检查分销功能开关
        distributionService.checkDistributionSetting();

        //获取分销佣金信息
        DistributionCash distributorCash = this.getById(id);
        //只有分销员和分销佣金记录存在的情况才可以审核
        if (distributorCash != null) {
            //获取分销员
            Distribution distribution = distributionService.getById(distributorCash.getDistributionId());
            if (distribution != null && distributorCash != null && distribution.getDistributionStatus().equals(DistributionStatusEnum.PASS.name())) {
                //审核通过
                if (result.equals(DistributionCashStatusEnum.PASS.name())) {
                    //审核通过需要校验冻结金额不足情况
                    if (distribution.getCommissionFrozen() < distributorCash.getPrice()) {
                        throw new ServiceException(ResultCode.WALLET_WITHDRAWAL_INSUFFICIENT);
                    }
                    //分销员佣金解冻
                    distribution.setCommissionFrozen(CurrencyUtil.sub(distribution.getCommissionFrozen(), distributorCash.getPrice()));
                    //分销记录操作
                    distributorCash.setDistributionCashStatus(DistributionCashStatusEnum.PASS.name());
                    distributorCash.setPayTime(new Date());
                    //提现到余额
                    memberWalletService.increase(distributorCash.getPrice(), distribution.getMemberId(), "分销佣金提现到余额", DepositServiceTypeEnum.WALLET_COMMISSION.name());
                } else {
                    //分销员佣金解冻
                    distribution.setCommissionFrozen(CurrencyUtil.sub(distribution.getCommissionFrozen(), distributorCash.getPrice()));
                    //分销员可提现金额退回
                    distribution.setCanRebate(CurrencyUtil.add(distribution.getCanRebate(), distributorCash.getPrice()));
                    distributorCash.setDistributionCashStatus(DistributionCashStatusEnum.REFUSE.name());
                }
                //分销员金额相关处理
                distributionService.updateById(distribution);
                //修改分销提现申请
                this.updateById(distributorCash);
                return distributorCash;
            }
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }
        throw new ServiceException(ResultCode.DISTRIBUTION_CASH_NOT_EXIST);

    }

    @Override
    public Integer newDistributionCash() {
        QueryWrapper queryWrapper = Wrappers.query();
        queryWrapper.eq("distribution_cash_status", DistributionCashStatusEnum.APPLY.name());
        return this.count(queryWrapper);
    }
}