package cn.lili.common.security.context;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

/**
 * 给予用户上下文，初始化参数
 *
 * @author Chopper
 * @version v4.0
 * @Description:
 * @since 2020/11/14 20:30
 */
@Component
public class UserContextInit implements ApplicationRunner {

    /**
     * 用户信息holder，认证信息的获取者
     */
    @Autowired
    private AuthenticationHandler authenticationHandler;

    /**
     * 在项目加载时指定认证信息获取者
     * 默认是由spring 安全上下文中获取
     *
     * @param args
     * @throws Exception
     */
    @Override
    public void run(ApplicationArguments args) {
        //UserContext.setHolder(authenticationHandler);
    }
}
