package cn.lili.common.security.context;

import cn.lili.common.security.AuthUser;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

/**
 * 获取用户信息 处理
 *
 * @author Chopper
 * @version v4.0
 * @Description:
 * @since 2020/11/14 20:35
 */
@Component
public class AuthenticationHandler {

    /**
     * 获取当前用户信息
     *
     * @return
     */
    public AuthUser getAuthUser() {
        //获取spring security 权限信息，如果token有权限，在这里就会得到内容
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null) {
            return null;
        }
        Object object = authentication.getDetails();
        if (object instanceof AuthUser) {
            return (AuthUser) authentication.getDetails();
        }
        return null;
    }
}
