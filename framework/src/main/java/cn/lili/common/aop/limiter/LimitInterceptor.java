package cn.lili.common.aop.limiter;

import cn.lili.common.aop.limiter.annotation.LimitPoint;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import com.google.common.collect.ImmutableList;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.io.Serializable;

/**
 * 流量拦截
 *
 * @author Chopper
 */
@Aspect
@Configuration
@Slf4j
public class LimitInterceptor {
    private RedisTemplate<String, Serializable> redisTemplate;

    private DefaultRedisScript<Number> limitScript;

    @Autowired
    public void setRedisTemplate(RedisTemplate redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    @Autowired
    public void setLimitScript(DefaultRedisScript<Number> limitScript) {
        this.limitScript = limitScript;
    }

    @Before("@annotation(limitPointAnnotation)")
    public void interceptor(LimitPoint limitPointAnnotation) {
        LimitType limitType = limitPointAnnotation.limitType();
        String name = limitPointAnnotation.name();
        String key;
        int limitPeriod = limitPointAnnotation.period();
        int limitCount = limitPointAnnotation.limit();
        switch (limitType) {
            case CUSTOMER:
                key = limitPointAnnotation.key();
                break;
            default:
                key = limitPointAnnotation.key() + getIpAddress();
        }
        ImmutableList<String> keys = ImmutableList.of(StringUtils.join(limitPointAnnotation.prefix(), key));
        try {
            Number count = redisTemplate.execute(limitScript, keys, limitCount, limitPeriod);
            log.info("限制请求{}, 当前请求{},缓存key{}", limitCount, count.intValue(), key);
            //如果缓存里没有值，或者他的值小于限制频率
            if (count.intValue() >= limitCount) {
                throw new ServiceException(ResultCode.LIMIT_ERROR);
            }
        }
        //如果从redis中执行都值判定为空，则这里跳过
        catch (NullPointerException e) {
            return;
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            throw new RuntimeException("服务器异常，请稍后再试");
        }
    }


    /**
     * 默认unknown常量值
     */
    private static final String UNKNOWN = "unknown";

    /**
     * 获取ip
     * @return ip
     */
    public String getIpAddress() {
        HttpServletRequest request = ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes()).getRequest();
        String ip = request.getHeader("x-forwarded-for");
        if (ip == null || ip.length() == 0 || UNKNOWN.equalsIgnoreCase(ip)) {
            ip = request.getHeader("Proxy-Client-IP");
        }
        if (ip == null || ip.length() == 0 || UNKNOWN.equalsIgnoreCase(ip)) {
            ip = request.getHeader("WL-Proxy-Client-IP");
        }
        if (ip == null || ip.length() == 0 || UNKNOWN.equalsIgnoreCase(ip)) {
            ip = request.getRemoteAddr();
        }
        return ip;
    }
}