package cn.lili.modules.im.token;

import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.token.Token;
import cn.lili.common.security.token.TokenUtil;
import cn.lili.common.security.token.base.AbstractTokenGenerate;
import cn.lili.modules.im.entity.dos.Seat;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 会员token生成
 *
 * @author Chopper
 * @version v4.0
 * @since 2020/11/16 10:50
 */
@Component
public class SeatTokenGenerate extends AbstractTokenGenerate<Seat> {
    @Autowired
    private TokenUtil tokenUtil;

    @Override
    public Token createToken(Seat seat, Boolean longTerm) {
        AuthUser authUser = AuthUser.builder()
                .username(seat.getUsername())
                .id(seat.getId())
                .nickName(seat.getNickName())
                .face(seat.getFace())
                .role(UserEnums.SEAT)
                .longTerm(longTerm)
                .tenantId(seat.getTenantId())
                .build();

        //登陆成功生成token
        return tokenUtil.createToken(authUser);
    }

    @Override
    public Token refreshToken(String refreshToken) {
        return tokenUtil.refreshToken(refreshToken);
    }

}
