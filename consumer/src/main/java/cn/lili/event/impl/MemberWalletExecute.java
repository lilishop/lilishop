package cn.lili.event.impl;


import cn.lili.event.MemberWithdrawalEvent;
import cn.lili.modules.wallet.entity.dto.MemberWalletUpdateDTO;
import cn.lili.modules.wallet.entity.dto.MemberWithdrawalMessage;
import cn.lili.modules.wallet.entity.enums.DepositServiceTypeEnum;
import cn.lili.modules.wallet.entity.enums.WithdrawStatusEnum;
import cn.lili.modules.wallet.service.MemberWalletService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 会员钱包创建
 *
 * @author Chopper
 * @since 2020-07-03 11:20
 */
@Service
public class MemberWalletExecute implements MemberWithdrawalEvent {

    @Autowired
    private MemberWalletService memberWalletService;

    @Override
    public void memberWithdrawal(MemberWithdrawalMessage memberWithdrawalMessage) {
        switch (WithdrawStatusEnum.valueOf(memberWithdrawalMessage.getStatus())) {
            case VIA_AUDITING:
                memberWalletService.withdrawal(memberWithdrawalMessage.getMemberWithdrawApplyId());
                break;
            case SUCCESS:
                //提现成功扣减冻结金额
                memberWalletService.reduceFrozen(
                        new MemberWalletUpdateDTO(memberWithdrawalMessage.getPrice(), memberWithdrawalMessage.getMemberId(), "提现成功，余额提现", DepositServiceTypeEnum.WALLET_WITHDRAWAL.name()));
                break;
            case ERROR:
                //需要从冻结金额扣减到余额
                memberWalletService.increaseWithdrawal(new MemberWalletUpdateDTO(memberWithdrawalMessage.getPrice(), memberWithdrawalMessage.getMemberId(), "提现失败，提现金额解冻到余额", DepositServiceTypeEnum.WALLET_WITHDRAWAL.name()));
                break;
            case FAIL_AUDITING:
                //需要从冻结金额扣减到余额
                memberWalletService.increaseWithdrawal(new MemberWalletUpdateDTO(memberWithdrawalMessage.getPrice(), memberWithdrawalMessage.getMemberId(), "审核拒绝，提现金额解冻到余额", DepositServiceTypeEnum.WALLET_WITHDRAWAL.name()));
            default:
                break;
        }
    }
}
