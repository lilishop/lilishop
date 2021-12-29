package cn.lili.modules.member.token;

import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.token.Token;
import cn.lili.common.security.token.TokenUtil;
import cn.lili.common.security.token.base.AbstractTokenGenerate;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Date;

/**
 * 会员token生成
 *
 * @author Chopper
 * @version v4.0
 * @since 2020/11/16 10:50
 */
@Component
public class MemberTokenGenerate extends AbstractTokenGenerate {

    @Autowired
    private MemberService memberService;
    @Autowired
    private TokenUtil tokenUtil;

    @Override
    public Token createToken(String username, Boolean longTerm) {

        Member member = memberService.findByUsername(username);

        //获取客户端类型
        String clientType = ThreadContextHolder.getHttpRequest().getHeader("clientType");
        ClientTypeEnum clientTypeEnum;
        try {
            //如果客户端为空，则缺省值为PC，pc第三方登录时不会传递此参数
            if (clientType == null) {
                clientTypeEnum = ClientTypeEnum.PC;
            } else {
                clientTypeEnum = ClientTypeEnum.valueOf(clientType);
            }
        } catch (IllegalArgumentException e) {
            clientTypeEnum = ClientTypeEnum.UNKNOWN;
        }
        //记录最后登录时间，客户端类型
        member.setLastLoginDate(new Date());
        member.setClientEnum(clientTypeEnum.name());
        memberService.updateById(member);

        AuthUser authUser = new AuthUser(member.getUsername(), member.getId(), member.getNickName(), member.getFace(), UserEnums.MEMBER);
        //登陆成功生成token
        return tokenUtil.createToken(username, authUser, longTerm, UserEnums.MEMBER);
    }

    @Override
    public Token refreshToken(String refreshToken) {
        return tokenUtil.refreshToken(refreshToken, UserEnums.MEMBER);
    }

}
