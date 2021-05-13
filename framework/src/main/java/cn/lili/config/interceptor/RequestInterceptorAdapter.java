package cn.lili.config.interceptor;

import cn.lili.config.context.ThreadContextHolder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * 写入request/response
 *
 * @author Chopper
 * @version v1.0
 * @since 2020-06-13 13:38
 */
@Slf4j
@Component
public class RequestInterceptorAdapter extends HandlerInterceptorAdapter {


    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response,
                             Object handler) {
        return true;
    }

    @Override
    public void postHandle(HttpServletRequest request, HttpServletResponse response,
                           Object handler, ModelAndView modelAndView) throws Exception {

        ThreadContextHolder.setHttpResponse(response);
        ThreadContextHolder.setHttpRequest(request);
    }

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response,
                                Object handler, Exception ex) throws Exception {
        ThreadContextHolder.remove();

        super.afterCompletion(request, response, handler, ex);
    }
}
