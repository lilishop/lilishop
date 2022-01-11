package cn.lili.common.security.token.base;

import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.token.Token;

/**
 * AbstractToken
 * 抽象token，定义生成token类
 *
 * @author Chopper
 * @version v1.0
 * 2020-11-13 10:13
 */
public abstract class AbstractTokenGenerate<T> {

    /**
     * 生成token
     *
     * @param user 用户名
     * @param longTerm 是否长时间有效
     * @return TOKEN对象
     */
    public abstract Token createToken(T user, Boolean longTerm);

    /**
     * 刷新token
     *
     * @param refreshToken 刷新token
     * @return token
     */
    public abstract Token refreshToken(String refreshToken);

    /**
     * 默认role
     */
    public UserEnums role = UserEnums.MANAGER;

}
