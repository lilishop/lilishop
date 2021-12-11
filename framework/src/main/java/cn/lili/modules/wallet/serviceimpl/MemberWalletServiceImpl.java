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
import cn.lili.modules.wallet.entity.enums.MemberWithdrawalDestinationEnum;
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
@Transactional(rollbackFor = Exception.class)
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

    @Override
    public MemberWalletVO getMemberWallet(String memberId) {
        //构建查询条件
        QueryWrapper<MemberWallet> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("member_id", memberId);
        //执行查询
        MemberWallet memberWallet = this.baseMapper.selectOne(queryWrapper);
        //如果没有钱包，则创建钱包
        if (memberWallet == null) {
            memberWallet = this.save(memberId, memberService.getById(memberId).getUsername());
        }
        //返回查询数据
        return new MemberWalletVO(memberWallet.getMemberWallet(), memberWallet.getMemberFrozenWallet());
    }

    @Override
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
    public Boolean reduceFrozen(MemberWalletUpdateDTO memberWalletUpdateDTO) {
        //检测会员预存款讯息是否存在，如果不存在则新建
        MemberWallet memberWallet = this.checkMemberWallet(memberWalletUpdateDTO.getMemberId());
        //校验此金额是否超过冻结金额
        if (0 > CurrencyUtil.sub(memberWallet.getMemberWallet(), memberWalletUpdateDTO.getMoney())) {
            throw new ServiceException(ResultCode.WALLET_WITHDRAWAL_INSUFFICIENT);
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
        MemberWallet memberWallet = this.getOne(new QueryWrapper<MemberWallet>().eq("member_id", memberId));
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
     * 1、先执行平台逻辑，平台逻辑成功后扣减第三方余额，顺序问题为了防止第三方提现成功，平台逻辑失败导致第三方零钱已提现，而我们商城余额未扣减
     * 2、如果余额扣减失败 则抛出异常，事务回滚
     *
     * @param price 提现金额
     * @return
     */
    @Override
    public Boolean applyWithdrawal(Double price) {
        MemberWithdrawalMessage memberWithdrawalMessage = new MemberWithdrawalMessage();
        AuthUser authUser = UserContext.getCurrentUser();
        //构建审核参数
        MemberWithdrawApply memberWithdrawApply = new MemberWithdrawApply();
        memberWithdrawApply.setMemberId(authUser.getId());
        memberWithdrawApply.setMemberName(authUser.getNickName());
        memberWithdrawApply.setApplyMoney(price);
        memberWithdrawApply.setApplyStatus(WithdrawStatusEnum.APPLY.name());
        memberWithdrawApply.setSn("W" + SnowFlake.getId());
        //校验该次提现是否需要审核,如果未进行配置 默认是需要审核
        Setting setting = settingService.get(SettingEnum.WITHDRAWAL_SETTING.name());
        if (setting != null) {
            //如果不需要审核则审核自动通过
            WithdrawalSetting withdrawalSetting = new Gson().fromJson(setting.getSettingValue(), WithdrawalSetting.class);
            if (!withdrawalSetting.getApply()) {
                memberWithdrawApply.setApplyStatus(WithdrawStatusEnum.VIA_AUDITING.name());
                memberWithdrawApply.setInspectRemark("系统自动审核通过");
                //校验金额是否满足提现，因为是从余额扣减，所以校验的是余额
                MemberWalletVO memberWalletVO = this.getMemberWallet(memberWithdrawApply.getMemberId());
                if (memberWalletVO.getMemberWallet() < price) {
                    throw new ServiceException(ResultCode.WALLET_WITHDRAWAL_INSUFFICIENT);
                }
                memberWithdrawalMessage.setStatus(WithdrawStatusEnum.VIA_AUDITING.name());
                //微信零钱提现
                Boolean result = withdrawal(memberWithdrawApply);
                if (result) {
                    this.reduce(new MemberWalletUpdateDTO(price, authUser.getId(), "余额提现成功", DepositServiceTypeEnum.WALLET_WITHDRAWAL.name()));
                }
            } else {
                memberWithdrawalMessage.setStatus(WithdrawStatusEnum.APPLY.name());
                //扣减余额到冻结金额
                this.reduceWithdrawal(new MemberWalletUpdateDTO(price, authUser.getId(), "提现金额已冻结，审核成功后到账", DepositServiceTypeEnum.WALLET_WITHDRAWAL.name()));
            }
            //发送余额提现申请消息

            memberWithdrawalMessage.setMemberId(authUser.getId());
            memberWithdrawalMessage.setPrice(price);
            memberWithdrawalMessage.setDestination(MemberWithdrawalDestinationEnum.WECHAT.name());
            String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_WITHDRAWAL.name();
            rocketMQTemplate.asyncSend(destination, memberWithdrawalMessage, RocketmqSendCallbackBuilder.commonCallback());
        }
        return memberWithdrawApplyService.save(memberWithdrawApply);
    }

    @Override
    public Boolean withdrawal(MemberWithdrawApply memberWithdrawApply) {
        memberWithdrawApply.setInspectTime(new Date());
        //保存或者修改零钱提现
        this.memberWithdrawApplyService.saveOrUpdate(memberWithdrawApply);
        //TODO 调用自动提现接口
        boolean result = true;
        //如果微信提现失败 则抛出异常 回滚数据
        if (!result) {
            throw new ServiceException(ResultCode.WALLET_ERROR_INSUFFICIENT);
        }
        return result;
    }

}