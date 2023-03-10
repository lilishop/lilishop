package cn.lili.modules.wallet.serviceimpl;


import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.CashierSupport;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.WithdrawalSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wallet.entity.dos.MemberWallet;
import cn.lili.modules.wallet.entity.dos.MemberWithdrawApply;
import cn.lili.modules.wallet.entity.dos.WalletLog;
import cn.lili.modules.wallet.entity.dto.MemberWalletUpdateDTO;
import cn.lili.modules.wallet.entity.dto.MemberWithdrawalMessage;
import cn.lili.modules.wallet.entity.enums.DepositServiceTypeEnum;
import cn.lili.modules.wallet.entity.enums.WithdrawStatusEnum;
import cn.lili.modules.wallet.entity.vo.MemberWalletVO;
import cn.lili.modules.wallet.mapper.MemberWalletMapper;
import cn.lili.modules.wallet.service.MemberWalletService;
import cn.lili.modules.wallet.service.MemberWithdrawApplyService;
import cn.lili.modules.wallet.service.WalletLogService;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.rocketmq.tags.MemberTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.google.gson.Gson;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;


/**
 * 会员余额业务层实现
 *
 * @author pikachu
 * @since 2020-02-25 14:10:16
 */
@Service
public class MemberWalletServiceImpl extends ServiceImpl<MemberWalletMapper, MemberWallet> implements MemberWalletService {

    @Autowired
    private RocketMQTemplate rocketMQTemplate;

    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    /**
     * 预存款日志
     */
    @Autowired
    private WalletLogService walletLogService;
    /**
     * 设置
     */
    @Autowired
    private SettingService settingService;
    /**
     * 会员
     */
    @Autowired
    private MemberService memberService;
    /**
     * 会员提现申请
     */
    @Autowired
    private MemberWithdrawApplyService memberWithdrawApplyService;
    @Autowired
    private CashierSupport cashierSupport;

    @Override
    public MemberWalletVO getMemberWallet(String memberId) {
        //构建查询条件
        QueryWrapper<MemberWallet> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("member_id", memberId);
        //执行查询
        MemberWallet memberWallet = this.getOne(queryWrapper, false);
        //如果没有钱包，则创建钱包
        if (memberWallet == null) {
            memberWallet = this.save(memberId, memberService.getById(memberId).getUsername());
        }
        //返回查询数据
        return new MemberWalletVO(memberWallet.getMemberWallet(), memberWallet.getMemberFrozenWallet());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean increaseWithdrawal(MemberWalletUpdateDTO memberWalletUpdateDTO) {
        //检测会员预存款讯息是否存在，如果不存在则新建
        MemberWallet memberWallet = this.checkMemberWallet(memberWalletUpdateDTO.getMemberId());
        //余额变动
        memberWallet.setMemberWallet(CurrencyUtil.add(memberWallet.getMemberWallet(), memberWalletUpdateDTO.getMoney()));
        memberWallet.setMemberFrozenWallet(CurrencyUtil.sub(memberWallet.getMemberFrozenWallet(), memberWalletUpdateDTO.getMoney()));
        this.updateById(memberWallet);
        //新增预存款日志
        WalletLog walletLog = new WalletLog(memberWallet.getMemberName(), memberWalletUpdateDTO);
        walletLogService.save(walletLog);
        return true;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean increase(MemberWalletUpdateDTO memberWalletUpdateDTO) {
        //检测会员预存款讯息是否存在，如果不存在则新建
        MemberWallet memberWallet = this.checkMemberWallet(memberWalletUpdateDTO.getMemberId());
        //新增预存款
        memberWallet.setMemberWallet(CurrencyUtil.add(memberWallet.getMemberWallet(), memberWalletUpdateDTO.getMoney()));
        this.baseMapper.updateById(memberWallet);
        //新增预存款日志
        WalletLog walletLog = new WalletLog(memberWallet.getMemberName(), memberWalletUpdateDTO);
        walletLogService.save(walletLog);
        return true;
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean reduce(MemberWalletUpdateDTO memberWalletUpdateDTO) {
        //检测会员预存款讯息是否存在，如果不存在则新建
        MemberWallet memberWallet = this.checkMemberWallet(memberWalletUpdateDTO.getMemberId());
        //减少预存款，需要校验 如果不够扣减预存款
        if (0 > CurrencyUtil.sub(memberWallet.getMemberWallet(), memberWalletUpdateDTO.getMoney())) {
            return false;
        }
        memberWallet.setMemberWallet(CurrencyUtil.sub(memberWallet.getMemberWallet(), memberWalletUpdateDTO.getMoney()));
        //保存记录
        this.updateById(memberWallet);
        //新增预存款日志
        WalletLog walletLog = new WalletLog(memberWallet.getMemberName(), memberWalletUpdateDTO, true);
        walletLogService.save(walletLog);
        return true;
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean reduceWithdrawal(MemberWalletUpdateDTO memberWalletUpdateDTO) {
        //检测会员预存款讯息是否存在，如果不存在则新建
        MemberWallet memberWallet = this.checkMemberWallet(memberWalletUpdateDTO.getMemberId());
        //减少预存款，需要校验 如果不够扣减预存款
        if (0 > CurrencyUtil.sub(memberWallet.getMemberWallet(), memberWalletUpdateDTO.getMoney())) {
            throw new ServiceException(ResultCode.WALLET_WITHDRAWAL_INSUFFICIENT);
        }
        memberWallet.setMemberWallet(CurrencyUtil.sub(memberWallet.getMemberWallet(), memberWalletUpdateDTO.getMoney()));
        memberWallet.setMemberFrozenWallet(CurrencyUtil.add(memberWallet.getMemberFrozenWallet(), memberWalletUpdateDTO.getMoney()));
        //修改余额
        this.updateById(memberWallet);
        //新增预存款日志
        WalletLog walletLog = new WalletLog(memberWallet.getMemberName(), memberWalletUpdateDTO, true);
        walletLogService.save(walletLog);
        return true;

    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean reduceFrozen(MemberWalletUpdateDTO memberWalletUpdateDTO) {
        //检测会员预存款讯息是否存在，如果不存在则新建
        MemberWallet memberWallet = this.checkMemberWallet(memberWalletUpdateDTO.getMemberId());
        //校验此金额是否超过冻结金额
        if (0 > CurrencyUtil.sub(memberWallet.getMemberFrozenWallet(), memberWalletUpdateDTO.getMoney())) {
            throw new ServiceException(ResultCode.WALLET_WITHDRAWAL_FROZEN_AMOUNT_INSUFFICIENT);
        }
        memberWallet.setMemberFrozenWallet(CurrencyUtil.sub(memberWallet.getMemberFrozenWallet(), memberWalletUpdateDTO.getMoney()));
        this.updateById(memberWallet);
        //新增预存款日志
        WalletLog walletLog = new WalletLog(memberWallet.getMemberName(), memberWalletUpdateDTO, true);
        walletLogService.save(walletLog);
        return true;
    }

    /**
     * 检测会员预存款是否存在，如果不存在则新建
     *
     * @param memberId 会员id
     */
    private MemberWallet checkMemberWallet(String memberId) {
        //获取会员预存款信息
        MemberWallet memberWallet = this.getOne(new QueryWrapper<MemberWallet>().eq("member_id", memberId), false);
        //如果会员预存款信息不存在则同步重新建立预存款信息
        if (memberWallet == null) {
            Member member = memberService.getById(memberId);
            if (member != null) {
                memberWallet = this.save(memberId, member.getUsername());
            } else {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
        }
        return memberWallet;
    }

    @Override
    public void setMemberWalletPassword(Member member, String password) {
        //对密码进行加密
        String pwd = new BCryptPasswordEncoder().encode(password);
        //校验会员预存款是否存在
        QueryWrapper<MemberWallet> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("member_id", member.getId());
        MemberWallet memberWallet = this.getOne(queryWrapper);
        //如果 预存款信息不为空 执行设置密码
        if (memberWallet != null) {
            memberWallet.setWalletPassword(pwd);
            this.updateById(memberWallet);
        }
    }


    @Override
    public Boolean checkPassword() {
        //获取当前登录会员
        AuthUser authUser = UserContext.getCurrentUser();
        //构建查询条件
        QueryWrapper<MemberWallet> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("member_id", authUser.getId());
        MemberWallet wallet = this.getOne(queryWrapper);
        return wallet != null && !StringUtils.isEmpty(wallet.getWalletPassword());
    }

    @Override
    public MemberWallet save(String memberId, String memberName) {
        //获取会员预存款信息
        MemberWallet memberWallet = this.getOne(new QueryWrapper<MemberWallet>().eq("member_id", memberId));
        if (memberWallet != null) {
            return memberWallet;
        }
        memberWallet = new MemberWallet();
        memberWallet.setMemberId(memberId);
        memberWallet.setMemberName(memberName);
        memberWallet.setMemberWallet(0D);
        memberWallet.setMemberFrozenWallet(0D);
        this.save(memberWallet);
        return memberWallet;
    }

    /**
     * 提现方法
     * 1、提现申请冻结用户的余额。
     * 2、判断是否需要平台审核。不需要审核则直接调用第三方提现，需要审核则审核通过后调用第三方提现
     *
     * @param price 提现金额
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean applyWithdrawal(Double price, String realName, String connectNumber) {

        if (price == null || price <= 0 || price > 1000000) {
            throw new ServiceException(ResultCode.WALLET_WITHDRAWAL_AMOUNT_ERROR);
        }
        AuthUser authUser = UserContext.getCurrentUser();

        //校验金额是否满足提现，因为是从余额扣减，所以校验的是余额
        MemberWalletVO memberWalletVO = this.getMemberWallet(authUser.getId());
        if (memberWalletVO.getMemberWallet() < price) {
            throw new ServiceException(ResultCode.WALLET_WITHDRAWAL_INSUFFICIENT);
        }
        //获取提现设置
        Setting setting = settingService.get(SettingEnum.WITHDRAWAL_SETTING.name());
        WithdrawalSetting withdrawalSetting = new Gson().fromJson(setting.getSettingValue(), WithdrawalSetting.class);

        //判断金额是否小于最低提现金额
        if (price < withdrawalSetting.getMinPrice()) {
            throw new ServiceException(ResultCode.WALLET_APPLY_MIN_PRICE_ERROR.message());
        }

        //构建审核参数
        MemberWithdrawApply memberWithdrawApply = new MemberWithdrawApply();
        memberWithdrawApply.setMemberId(authUser.getId());
        memberWithdrawApply.setMemberName(authUser.getNickName());
        memberWithdrawApply.setApplyMoney(price);
        memberWithdrawApply.setRealName(realName);
        memberWithdrawApply.setConnectNumber(connectNumber);

        //判断提现是否需要审核
        if (withdrawalSetting.getApply()) {
            memberWithdrawApply.setApplyStatus(WithdrawStatusEnum.APPLY.name());
        } else {
            memberWithdrawApply.setApplyStatus(WithdrawStatusEnum.VIA_AUDITING.name());
        }

        memberWithdrawApply.setSn("W" + SnowFlake.getId());

        //添加提现申请记录
        memberWithdrawApplyService.save(memberWithdrawApply);

        //扣减余额到冻结金额
        this.reduceWithdrawal(new MemberWalletUpdateDTO(price, authUser.getId(), "提现金额已冻结", DepositServiceTypeEnum.WALLET_WITHDRAWAL.name()));

        //发送余额提现申请消息
        MemberWithdrawalMessage memberWithdrawalMessage = new MemberWithdrawalMessage();
        memberWithdrawalMessage.setMemberWithdrawApplyId(memberWithdrawApply.getId());
        memberWithdrawalMessage.setStatus(memberWithdrawApply.getApplyStatus());
        memberWithdrawalMessage.setMemberId(authUser.getId());
        memberWithdrawalMessage.setPrice(price);
        String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_WITHDRAWAL.name();
        rocketMQTemplate.asyncSend(destination, memberWithdrawalMessage, RocketmqSendCallbackBuilder.commonCallback());

        return true;
    }

    @Override
    public Boolean withdrawal(String withdrawApplyId) {
        MemberWithdrawApply memberWithdrawApply = memberWithdrawApplyService.getById(withdrawApplyId);
        memberWithdrawApply.setInspectTime(new Date());
        //获取提现设置
        Setting setting = settingService.get(SettingEnum.WITHDRAWAL_SETTING.name());
        WithdrawalSetting withdrawalSetting = new Gson().fromJson(setting.getSettingValue(), WithdrawalSetting.class);

        //调用提现方法
        boolean result = true;
        if ("WECHAT".equals(withdrawalSetting.getType())) {
            result = cashierSupport.transfer(PaymentMethodEnum.WECHAT, memberWithdrawApply);
        } else if ("ALI".equals(withdrawalSetting.getType())) {
            result = cashierSupport.transfer(PaymentMethodEnum.ALIPAY, memberWithdrawApply);
        }

        //成功则扣减冻结金额
        //失败则恢复冻结金额

        if (result) {
            memberWithdrawApply.setApplyStatus(WithdrawStatusEnum.SUCCESS.name());
        } else {
            memberWithdrawApply.setApplyStatus(WithdrawStatusEnum.ERROR.name());
        }
        //修改提现申请
        this.memberWithdrawApplyService.updateById(memberWithdrawApply);

        //发送余额提现申请消息
        MemberWithdrawalMessage memberWithdrawalMessage = new MemberWithdrawalMessage();
        memberWithdrawalMessage.setMemberWithdrawApplyId(memberWithdrawApply.getId());
        memberWithdrawalMessage.setStatus(memberWithdrawApply.getApplyStatus());
        memberWithdrawalMessage.setMemberId(memberWithdrawApply.getMemberId());
        memberWithdrawalMessage.setPrice(memberWithdrawApply.getApplyMoney());
        memberWithdrawalMessage.setStatus(memberWithdrawApply.getApplyStatus());

        String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_WITHDRAWAL.name();
        rocketMQTemplate.asyncSend(destination, memberWithdrawalMessage, RocketmqSendCallbackBuilder.commonCallback());
        return result;
    }

}