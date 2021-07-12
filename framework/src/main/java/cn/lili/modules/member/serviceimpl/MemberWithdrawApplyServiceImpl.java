package cn.lili.modules.member.serviceimpl;


import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.MemberWithdrawApply;
import cn.lili.modules.member.entity.enums.WithdrawStatusEnum;
import cn.lili.modules.member.entity.vo.MemberWalletVO;
import cn.lili.modules.member.entity.vo.MemberWithdrawApplyQueryVO;
import cn.lili.modules.member.mapper.MemberWithdrawApplyMapper;
import cn.lili.modules.member.service.MemberWalletService;
import cn.lili.modules.member.service.MemberWithdrawApplyService;
import cn.lili.modules.order.trade.entity.enums.DepositServiceTypeEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;


/**
 * 会员提现申请业务层实现
 *
 * @author pikachu
 * @date 2020-02-25 14:10:16
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class MemberWithdrawApplyServiceImpl extends ServiceImpl<MemberWithdrawApplyMapper, MemberWithdrawApply> implements MemberWithdrawApplyService {

    /**
     * 会员余额
     */
    @Autowired
    private MemberWalletService memberWalletService;

    @Override
    public Boolean audit(String applyId, Boolean result, String remark) {
        //查询申请记录
        MemberWithdrawApply memberWithdrawApply = this.getById(applyId);
        memberWithdrawApply.setInspectRemark(remark);
        if (memberWithdrawApply != null) {
            //写入备注
            memberWithdrawApply.setInspectRemark(remark);
            //如果审核通过 则微信直接提现，反之则记录审核状态
            if (result) {
                //校验金额是否满足提现，因为是从冻结金额扣减，所以校验的是冻结金额
                MemberWalletVO memberWalletVO = memberWalletService.getMemberWallet(memberWithdrawApply.getMemberId());
                if (memberWalletVO.getMemberFrozenWallet() < memberWithdrawApply.getApplyMoney()) {
                    throw new ServiceException(ResultCode.WALLET_WITHDRAWAL_INSUFFICIENT);
                }
                memberWithdrawApply.setApplyStatus(WithdrawStatusEnum.VIA_AUDITING.name());
                //保存审核记录
                this.updateById(memberWithdrawApply);
                //提现，微信提现成功后扣减冻结金额
                Boolean bool = memberWalletService.withdrawal(memberWithdrawApply);
                if (bool) {
                    memberWalletService.reduceFrozen(memberWithdrawApply.getApplyMoney(), memberWithdrawApply.getMemberId(), "审核通过，余额提现", DepositServiceTypeEnum.WALLET_WITHDRAWAL.name());
                    return true;
                }
            } else {
                //如果审核拒绝 审核备注必填
                if (StringUtils.isEmpty(remark)) {
                    throw new ServiceException(ResultCode.WALLET_REMARK_ERROR);
                }
                memberWithdrawApply.setApplyStatus(WithdrawStatusEnum.FAIL_AUDITING.name());
                this.updateById(memberWithdrawApply);
                //需要从冻结金额扣减到余额
                memberWalletService.increaseWithdrawal(memberWithdrawApply.getApplyMoney(), memberWithdrawApply.getMemberId(), "审核拒绝，提现金额解冻到余额", DepositServiceTypeEnum.WALLET_WITHDRAWAL.name());
                return true;
            }
        }
        throw new ServiceException(ResultCode.WALLET_APPLY_ERROR);
    }


    @Override
    public IPage<MemberWithdrawApply> getMemberWithdrawPage(PageVO pageVO, MemberWithdrawApplyQueryVO memberWithdrawApplyQueryVO) {
        //构建查询条件
        QueryWrapper<MemberWithdrawApply> queryWrapper = new QueryWrapper<>();
        //会员名称
        queryWrapper.like(!StringUtils.isEmpty(memberWithdrawApplyQueryVO.getMemberName()), "member_name", memberWithdrawApplyQueryVO.getMemberName());
        //充值订单号
        queryWrapper.eq(!StringUtils.isEmpty(memberWithdrawApplyQueryVO.getSn()), "sn", memberWithdrawApplyQueryVO.getSn());
        //会员id
        queryWrapper.eq(!StringUtils.isEmpty(memberWithdrawApplyQueryVO.getMemberId()), "member_id", memberWithdrawApplyQueryVO.getMemberId());
        //已付款的充值订单
        queryWrapper.eq(!StringUtils.isEmpty(memberWithdrawApplyQueryVO.getApplyStatus()), "apply_status", memberWithdrawApplyQueryVO.getApplyStatus());
        //开始时间和结束时间
        if (!StringUtils.isEmpty(memberWithdrawApplyQueryVO.getStartDate()) && !StringUtils.isEmpty(memberWithdrawApplyQueryVO.getEndDate())) {
            Date start = cn.hutool.core.date.DateUtil.parse(memberWithdrawApplyQueryVO.getStartDate());
            Date end = cn.hutool.core.date.DateUtil.parse(memberWithdrawApplyQueryVO.getEndDate());
            queryWrapper.between("create_time", start, end);
        }
        queryWrapper.orderByDesc("create_time");
        //查询返回数据
        return this.baseMapper.selectPage(PageUtil.initPage(pageVO), queryWrapper);
    }
}