package cn.lili.common.token.base.generate;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.token.Token;
import cn.lili.common.token.TokenUtil;
import cn.lili.common.token.base.AbstractTokenGenerate;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.service.StoreService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 商家token生成
 *
 * @author Chopper
 * @version v4.0
 * @since 2020/11/16 10:51
 */
@Component
public class StoreTokenGenerate extends AbstractTokenGenerate {
    @Autowired
    private MemberService memberService;
    @Autowired
    private StoreService storeService;
    @Autowired
    private TokenUtil tokenUtil;

    @Override
    public Token createToken(String username, Boolean longTerm) {
        //生成token
        Member member = memberService.findByUsername(username);
        if (!member.getHaveStore()) {
            throw new ServiceException(ResultCode.STORE_NOT_OPEN);
        }
        AuthUser user = new AuthUser(member.getUsername(), member.getId(), member.getNickName(), UserEnums.STORE);
        LambdaQueryWrapper<Store> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Store::getMemberId, member.getId());
        Store store = storeService.getOne(queryWrapper);
        user.setStoreId(store.getId());
        user.setStoreName(store.getStoreName());
        return tokenUtil.createToken(username, user, longTerm, UserEnums.STORE);
    }

    @Override
    public Token refreshToken(String refreshToken) {
        return tokenUtil.refreshToken(refreshToken, UserEnums.STORE);
    }

}
