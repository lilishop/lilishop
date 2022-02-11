package cn.lili.common.aop.interceptor;

/**
 * 防重复提交业务
 *
 * @author Chopper
 * @version v1.0
 * 2022-01-25 09:20
 */

import cn.lili.cache.Cache;
import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;

@Aspect
@Component
@Slf4j
public class PreventDuplicateSubmissionsInterceptor {

    @Autowired
    private Cache<String> cache;


    @Before("@annotation(preventDuplicateSubmissions)")
    public void interceptor(PreventDuplicateSubmissions preventDuplicateSubmissions) {

        try {
            Long count = cache.incr(getParams(), preventDuplicateSubmissions.expire());
            //如果超过1或者设置的参数，则表示重复提交了
            if (count.intValue() >= preventDuplicateSubmissions.expire()) {
                throw new ServiceException(ResultCode.LIMIT_ERROR);
            }
        }
        //如果参数为空，则表示用户未登录，直接略过，不做处理
        catch (NullPointerException e) {
            return;
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            throw new ServiceException(ResultCode.ERROR);
        }
    }

    /**
     * 获取表单参数
     *
     * @return
     */
    private String getParams() {
        HttpServletRequest request = ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes()).getRequest();
        //请求地址
        return request.getRequestURI() + UserContext.getCurrentUser().getId() + UserContext.getCurrentUser().getUsername();
    }


}
