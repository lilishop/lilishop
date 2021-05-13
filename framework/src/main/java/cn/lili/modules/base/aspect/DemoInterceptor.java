package cn.lili.modules.base.aspect;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.config.properties.SystemSetting;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 演示站点拦截
 * DemoInterceptor
 *
 * @author Chopper
 * @version v1.0
 * 2021-05-12 17:55
 */
@Component
@Aspect
public class DemoInterceptor {

    @Autowired
    private SystemSetting systemSetting;

    @Before("@annotation(demoSite)")
    public void doAfter(DemoSite demoSite) {
        if (systemSetting.getIsDemoSite()) {
            throw new ServiceException(ResultCode.DEMO_SITE_EXCEPTION);
        }
    }

}
