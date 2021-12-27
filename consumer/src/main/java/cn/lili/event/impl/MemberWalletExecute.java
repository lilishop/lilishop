package cn.lili.event.impl;


import cn.lili.event.MemberRegisterEvent;
import cn.lili.modules.member.entity.dos.Member;
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
public class MemberWalletExecute implements MemberRegisterEvent {

    @Autowired
    private MemberWalletService memberWalletService;

    @Override
    public void memberRegister(Member member) {
        // 有些情况下，会同时创建一个member_id的两条数据
//        memberWalletService.save(member.getId(),member.getUsername());
    }
}
