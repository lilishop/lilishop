package cn.lili.modules.wallet.serviceimpl;


import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.WithdrawalSetting;
import cn.lili.modules.system.entity.dto.payment.WechatPaymentSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wallet.entity.dos.MemberWithdrawApply;
import cn.lili.modules.wallet.entity.dto.MemberWithdrawalMessage;
import cn.lili.modules.wallet.entity.enums.WithdrawStatusEnum;
import cn.lili.modules.wallet.entity.vo.MemberWalletVO;
import cn.lili.modules.wallet.entity.vo.MemberWithdrawApplyQueryVO;
import cn.lili.modules.wallet.entity.vo.WechatTransferInfoVO;
import cn.lili.modules.wallet.mapper.MemberWithdrawApplyMapper;
import cn.lili.modules.wallet.service.MemberWalletService;
import cn.lili.modules.wallet.service.MemberWithdrawApplyService;
import cn.lili.mybatis.BaseEntity;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.message.MessageSendCallbackBuilder;
import cn.lili.rocketmq.tags.MemberTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.google.gson.Gson;
import cn.lili.message.LiliMessageTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;


/**
 * 客户提现申请业务层实现
 *
 * @author pikachu
 * @since 2020-02-25 14:10:16
 */
@Service
public class MemberWithdrawApplyServiceImpl extends ServiceImpl<MemberWithdrawApplyMapper, MemberWithdrawApply> implements MemberWithdrawApplyService {

    @Autowired
    private LiliMessageTemplate liliMessageTemplate;
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    @Autowired
    private SettingService settingService;

    /**
     * 客户余额
     */
    @Autowired
    @Lazy
    private MemberWalletService memberWalletService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean audit(String applyId, Boolean result, String remark) {
        MemberWithdrawalMessage memberWithdrawalMessage = new MemberWithdrawalMessage();
        //查询申请记录
        MemberWithdrawApply memberWithdrawApply = this.getById(applyId);
        memberWithdrawApply.setInspectRemark(remark);
        memberWithdrawApply.setInspectTime(new Date());
        if (memberWithdrawApply != null) {
            //获取账户余额
            MemberWalletVO memberWalletVO = memberWalletService.getMemberWallet(memberWithdrawApply.getMemberId());
            //校验金额是否满足提现，因为是从冻结金额扣减，所以校验的是冻结金额
            if (memberWalletVO.getMemberFrozenWallet() < memberWithdrawApply.getApplyMoney()) {
                throw new ServiceException(ResultCode.WALLET_WITHDRAWAL_FROZEN_AMOUNT_INSUFFICIENT);
            }
            //如果审核通过 则微信直接提现，反之则记录审核状态
            if (Boolean.TRUE.equals(result)) {
                memberWithdrawApply.setApplyStatus(WithdrawStatusEnum.VIA_AUDITING.name());

            } else {
                memberWithdrawApply.setApplyStatus(WithdrawStatusEnum.FAIL_AUDITING.name());
                //保存修改审核记录
                this.updateById(memberWithdrawApply);

            }
            //发送审核消息
            memberWithdrawalMessage.setStatus(memberWithdrawApply.getApplyStatus());
            memberWithdrawalMessage.setMemberWithdrawApplyId(memberWithdrawApply.getId());
            memberWithdrawalMessage.setMemberId(memberWithdrawApply.getMemberId());
            memberWithdrawalMessage.setPrice(memberWithdrawApply.getApplyMoney());

            String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_WITHDRAWAL.name();
            liliMessageTemplate.asyncSend(destination, memberWithdrawalMessage, MessageSendCallbackBuilder.commonCallback());
            return true;
        }
        throw new ServiceException(ResultCode.WALLET_APPLY_ERROR);
    }


    @Override
    public IPage<MemberWithdrawApply> getMemberWithdrawPage(PageVO pageVO, MemberWithdrawApplyQueryVO memberWithdrawApplyQueryVO) {
        //构建查询条件
        QueryWrapper<MemberWithdrawApply> queryWrapper = new QueryWrapper<>();
        //客户名称
        queryWrapper.like(!StringUtils.isEmpty(memberWithdrawApplyQueryVO.getMemberName()), "member_name", memberWithdrawApplyQueryVO.getMemberName());
        //充值订单号
        queryWrapper.eq(!StringUtils.isEmpty(memberWithdrawApplyQueryVO.getSn()), "sn", memberWithdrawApplyQueryVO.getSn());
        //客户id
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

    @Override
    public WechatTransferInfoVO getWechatTransferInfo(String id) {
        WechatTransferInfoVO wechatTransferInfoVO=new WechatTransferInfoVO();
        WithdrawalSetting withdrawalSetting = new Gson().fromJson(settingService.get(SettingEnum.WITHDRAWAL_SETTING.name()).getSettingValue(),
                WithdrawalSetting.class);
        Setting systemSetting = settingService.get(SettingEnum.WECHAT_PAYMENT.name());
        WechatPaymentSetting wechatPaymentSetting = JSONUtil.toBean(systemSetting.getSettingValue(), WechatPaymentSetting.class);
        wechatTransferInfoVO.setAppId(withdrawalSetting.getWechatAppId());
        wechatTransferInfoVO.setMchId(wechatPaymentSetting.getMchId());
        wechatTransferInfoVO.setWechatPackage(this.getById(id).getWechatPackage());
        return wechatTransferInfoVO;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void transferSuccess(String outBillNo) {
        if (StringUtils.isEmpty(outBillNo)) {
            return;
        }
        MemberWithdrawApply memberWithdrawApply = this.getOne(new LambdaQueryWrapper<MemberWithdrawApply>()
                .eq(MemberWithdrawApply::getOutBillNo, outBillNo)
                .eq(MemberWithdrawApply::getApplyStatus, WithdrawStatusEnum.WAIT_USER_CONFIRM.name()));
        if (memberWithdrawApply == null) {
            return;
        }
        boolean updated = this.update(new LambdaUpdateWrapper<MemberWithdrawApply>()
                .eq(MemberWithdrawApply::getId, memberWithdrawApply.getId())
                .eq(MemberWithdrawApply::getApplyStatus, WithdrawStatusEnum.WAIT_USER_CONFIRM.name())
                .set(MemberWithdrawApply::getApplyStatus, WithdrawStatusEnum.SUCCESS.name()));
        if (!updated) {
            return;
        }
        MemberWithdrawalMessage memberWithdrawalMessage = new MemberWithdrawalMessage();
        memberWithdrawalMessage.setMemberWithdrawApplyId(memberWithdrawApply.getId());
        memberWithdrawalMessage.setStatus(WithdrawStatusEnum.SUCCESS.name());
        memberWithdrawalMessage.setMemberId(memberWithdrawApply.getMemberId());
        memberWithdrawalMessage.setPrice(memberWithdrawApply.getApplyMoney());
        String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_WITHDRAWAL.name();
        liliMessageTemplate.asyncSend(destination, memberWithdrawalMessage, MessageSendCallbackBuilder.commonCallback());
    }
}
