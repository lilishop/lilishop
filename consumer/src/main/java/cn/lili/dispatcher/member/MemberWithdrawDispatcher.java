package cn.lili.dispatcher.member;

import cn.hutool.json.JSONUtil;
import cn.lili.dispatcher.MemberMqDispatcher;
import cn.lili.event.MemberWithdrawalEvent;
import cn.lili.modules.wallet.entity.dto.MemberWithdrawalMessage;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

import static cn.lili.rocketmq.tags.MemberTagsEnum.MEMBER_REGISTER;
import static cn.lili.rocketmq.tags.MemberTagsEnum.MEMBER_WITHDRAWAL;

@Slf4j
@RequiredArgsConstructor
@Component
public class MemberWithdrawDispatcher implements MemberMqDispatcher {

    /**
     * 会员提现
     */
    @Autowired
    private final List<MemberWithdrawalEvent> memberWithdrawalEvents;

    @Override
    public String supportTag() {
        return MEMBER_WITHDRAWAL.name();
    }

    @Override
    public void dispatch(MessageExt messageExt) {
        MemberWithdrawalMessage memberWithdrawalMessage = JSONUtil.toBean(new String(messageExt.getBody()), MemberWithdrawalMessage.class);
        for (MemberWithdrawalEvent memberWithdrawalEvent : memberWithdrawalEvents) {
            try {
                memberWithdrawalEvent.memberWithdrawal(memberWithdrawalMessage);
            } catch (Exception e) {
                log.error("执行会员提现相关分发逻辑出错 {}", e.getMessage());
            }
        }
    }
}
