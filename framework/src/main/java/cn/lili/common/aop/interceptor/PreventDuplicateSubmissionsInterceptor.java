package cn.lili.common.aop.interceptor;

/**
 * 防重复提交业务
 *
 * @author Chopper
 * @version v1.0
 * 2022-01-25 09:20
 */

import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
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
            String redisKey = getParams(preventDuplicateSubmissions.userIsolation());
            Long count = cache.incr(redisKey, preventDuplicateSubmissions.expire());
            log.debug("防重复提交：params-{},value-{}", redisKey, count);
            //如果超过0或者设置的参数，则表示重复提交了
            if (count.intValue() > 0) {
                throw new ServiceException(ResultCode.LIMIT_ERROR);
            }
        }
        //如果参数为空，则表示用户未登录，直接略过，不做处理
        catch (NullPointerException e) {
            return;
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            log.error("防重复提交拦截器异常", e);
            throw new ServiceException(ResultCode.ERROR);
        }
    }

    /**
     * 获取表单参数
     *
     * @param userIsolation 用户是否隔离
     * @return 计数器key
     */
    private String getParams(Boolean userIsolation) {
        HttpServletRequest request = ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes()).getRequest();
        StringBuilder stringBuilder = new StringBuilder();
        //拼接请求地址
        stringBuilder.append(request.getRequestURI());

        //参数不为空则拼接参数
        if (!request.getParameterMap().isEmpty()) {
            stringBuilder.append(JSONUtil.toJsonStr(request.getParameterMap()));
        }
        //用户隔离设置为开启，则选择当前用回顾
        if (userIsolation) {
            AuthUser authUser = UserContext.getCurrentUser();
            //用户为空则发出警告，但不拼接，否则拼接用户id
            if (authUser == null) {
                log.warn("user isolation settings are on,but current user is null");
            }
//           不为空则拼接用户id
            else {
                stringBuilder.append(authUser.getId());
            }
        }
        //请求地址
        return stringBuilder.toString();
    }


}
