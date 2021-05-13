package cn.lili.event.impl;


import cn.lili.event.MemberRegisterEvent;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberWalletService;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 会员钱包创建
 *
 * @author Chopper
 * @date 2020-07-03 11:20
 */
@Service
public class MemberWalletExecute implements MemberRegisterEvent {

    @Autowired
    private MemberWalletService memberWalletService;

    @Override
    public void memberRegister(Member member) {
        memberWalletService.save(member.getId(),member.getUsername());
    }
}
