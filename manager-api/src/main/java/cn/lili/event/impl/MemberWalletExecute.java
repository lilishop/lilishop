package cn.lili.event.impl;


import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.event.MemberWithdrawalEvent;
import cn.lili.modules.wallet.entity.dos.MemberWithdrawApply;
import cn.lili.modules.wallet.entity.dto.MemberWalletUpdateDTO;
import cn.lili.modules.wallet.entity.dto.MemberWithdrawalMessage;
import cn.lili.modules.wallet.entity.enums.DepositServiceTypeEnum;
import cn.lili.modules.wallet.entity.enums.WithdrawStatusEnum;
import cn.lili.modules.wallet.service.MemberWithdrawApplyService;
import cn.lili.modules.wallet.service.MemberWalletService;
import cn.lili.rocketmq.tags.MemberTagsEnum;
import lombok.extern.slf4j.Slf4j;
import cn.lili.message.LiliMessageTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.Message;
import org.springframework.messaging.support.MessageBuilder;
import org.springframework.stereotype.Service;

/**
 * ??????
 *
 * @author Chopper
 * @since 2020-07-03 11:20
 */
@Slf4j
@Service
public class MemberWalletExecute implements MemberWithdrawalEvent {

    private static final int WITHDRAWAL_DELAY_TIME_LEVEL = 5;
    private static final int WITHDRAWAL_RETRY_LIMIT = 1;

    @Autowired
    private MemberWalletService memberWalletService;

    @Autowired
    private MemberWithdrawApplyService memberWithdrawApplyService;

    @Autowired
    private LiliMessageTemplate liliMessageTemplate;

    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Override
    public void memberWithdrawal(MemberWithdrawalMessage memberWithdrawalMessage) {
        switch (WithdrawStatusEnum.valueOf(memberWithdrawalMessage.getStatus())) {
            case VIA_AUDITING:
                MemberWithdrawApply memberWithdrawApply = memberWithdrawApplyService.getById(memberWithdrawalMessage.getMemberWithdrawApplyId());
                if (memberWithdrawApply == null) {
                    this.retryWithdrawalLater(memberWithdrawalMessage);
                    break;
                }
                memberWalletService.withdrawal(memberWithdrawalMessage.getMemberWithdrawApplyId());
                break;
            case SUCCESS:
                //??????????
                memberWalletService.reduceFrozen(
                        new MemberWalletUpdateDTO(memberWithdrawalMessage.getPrice(), memberWithdrawalMessage.getMemberId(), "?????????",
                                DepositServiceTypeEnum.WALLET_WITHDRAWAL.name()));
                break;
            case ERROR:
                //????????????
                memberWalletService.increaseWithdrawal(new MemberWalletUpdateDTO(memberWithdrawalMessage.getPrice(),
                        memberWithdrawalMessage.getMemberId(), "?????????????????", DepositServiceTypeEnum.WALLET_WITHDRAWAL.name()));
                break;
            case FAIL_AUDITING:
                //????????????
                memberWalletService.increaseWithdrawal(new MemberWalletUpdateDTO(memberWithdrawalMessage.getPrice(),
                        memberWithdrawalMessage.getMemberId(), "??????????????", DepositServiceTypeEnum.WALLET_WITHDRAWAL.name()));
                break;
            default:
                break;
        }
    }

    private void retryWithdrawalLater(MemberWithdrawalMessage memberWithdrawalMessage) {
        int retryCount = memberWithdrawalMessage.getRetryCount() == null ? 0 : memberWithdrawalMessage.getRetryCount();
        if (retryCount >= WITHDRAWAL_RETRY_LIMIT) {
            log.error("?????????????????????????withdrawApplyId={}", memberWithdrawalMessage.getMemberWithdrawApplyId());
            return;
        }
        memberWithdrawalMessage.setRetryCount(retryCount + 1);
        String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_WITHDRAWAL.name();
        Message<MemberWithdrawalMessage> message = MessageBuilder.withPayload(memberWithdrawalMessage).build();
        liliMessageTemplate.syncSend(destination, message, 3000, WITHDRAWAL_DELAY_TIME_LEVEL);
        log.warn("???????????1??????withdrawApplyId={}, retryCount={}",
                memberWithdrawalMessage.getMemberWithdrawApplyId(), memberWithdrawalMessage.getRetryCount());
    }
}
