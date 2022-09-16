package cn.lili.modules.member.token;

import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.token.Token;
import cn.lili.common.security.token.TokenUtil;
import cn.lili.common.security.token.base.AbstractTokenGenerate;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.rocketmq.tags.MemberTagsEnum;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
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
public class MemberTokenGenerate extends AbstractTokenGenerate<Member> {
    @Autowired
    private TokenUtil tokenUtil;
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Autowired
    private RocketMQTemplate rocketMQTemplate;

    @Override
    public Token createToken(Member member, Boolean longTerm) {


        ClientTypeEnum clientTypeEnum;
        try {
            //获取客户端类型
            String clientType = ThreadContextHolder.getHttpRequest().getHeader("clientType");
            //如果客户端为空，则缺省值为PC，pc第三方登录时不会传递此参数
            if (clientType == null) {
                clientTypeEnum = ClientTypeEnum.PC;
            } else {
                clientTypeEnum = ClientTypeEnum.valueOf(clientType);
            }
        } catch (Exception e) {
            clientTypeEnum = ClientTypeEnum.UNKNOWN;
        }
        //记录最后登录时间，客户端类型
        member.setLastLoginDate(new Date());
        member.setClientEnum(clientTypeEnum.name());
        String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_LOGIN.name();
        rocketMQTemplate.asyncSend(destination, member, RocketmqSendCallbackBuilder.commonCallback());

        AuthUser authUser = new AuthUser(member.getUsername(), member.getId(), member.getNickName(), member.getFace(), UserEnums.MEMBER);
        //登陆成功生成token
        return tokenUtil.createToken(member.getUsername(), authUser, longTerm, UserEnums.MEMBER);
    }

    @Override
    public Token refreshToken(String refreshToken) {
        return tokenUtil.refreshToken(refreshToken, UserEnums.MEMBER);
    }

}
