package cn.lili.modules.system.aspect.interceptor;

import cn.lili.modules.system.aspect.annotation.SystemLogPoint;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.IpHelper;
import cn.lili.common.utils.SpelUtil;
import cn.lili.common.utils.ThreadPoolUtil;
import cn.lili.modules.permission.entity.vo.SystemLogVO;
import cn.lili.common.utils.IpUtils;
import cn.lili.modules.permission.service.SystemLogService;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.NamedThreadLocal;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletRequest;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * Spring AOP实现日志管理
 *
 * @author Chopper
 */
@Aspect
@Component
@Slf4j
public class SystemLogAspect {

    /**
     * 启动线程异步记录日志
     */
    private static final ThreadLocal<Date> BEGIN_TIME_THREAD_LOCAL = new NamedThreadLocal<>("SYSTEM-LOG");

    @Autowired
    private SystemLogService systemLogService;

    @Autowired
    private HttpServletRequest request;

    @Autowired
    private IpHelper ipHelper;

    /**
     * Controller层切点,注解方式
     */
    @Pointcut("@annotation(cn.lili.modules.system.aspect.annotation.SystemLogPoint)")
    public void controllerAspect() {

    }

    /**
     * 前置通知 (在方法执行之前返回)用于拦截Controller层记录用户的操作的开始时间
     */
    @Before("controllerAspect()")
    public void doBefore() {
        BEGIN_TIME_THREAD_LOCAL.set(new Date());
    }


    /**
     * 后置通知(在方法执行之后并返回数据) 用于拦截Controller层无异常的操作
     *
     * @param joinPoint 切点
     */
    @AfterReturning(returning = "rvt", pointcut = "controllerAspect()")
    public void after(JoinPoint joinPoint, Object rvt) {
        try {
            Map map = spelFormat(joinPoint, rvt);
            String description = map.get("description").toString();
            String customerLog = map.get("customerLog").toString();

            Map<String, String[]> logParams = request.getParameterMap();
            AuthUser authUser = UserContext.getCurrentUser();
            SystemLogVO systemLogVO = new SystemLogVO();

            if (authUser == null) {
                //如果是商家则记录商家id，否则记录-1，代表平台id
                systemLogVO.setStoreId(-2L);
                //请求用户
                systemLogVO.setUsername("游客");
            } else {
                //如果是商家则记录商家id，否则记录-1，代表平台id
                systemLogVO.setStoreId(authUser.getRole().equals(UserEnums.STORE) ? Long.parseLong(authUser.getStoreId()) : -1);
                //请求用户
                systemLogVO.setUsername(authUser.getUsername());
            }

            //日志标题
            systemLogVO.setName(description);
            //日志请求url
            systemLogVO.setRequestUrl(request.getRequestURI());
            //请求方式
            systemLogVO.setRequestType(request.getMethod());
            //请求参数
            systemLogVO.setMapToParams(logParams);
            //响应参数 此处数据太大了，所以先注释掉
//           systemLogVO.setResponseBody(JSONUtil.toJsonStr(rvt));
            //请求IP
            systemLogVO.setIp(IpUtils.getIpAddress(request));
            //IP地址
            systemLogVO.setIpInfo(ipHelper.getIpCity(request));
            //写入自定义日志内容
            systemLogVO.setCustomerLog(customerLog);
            //请求开始时间
            long beginTime = BEGIN_TIME_THREAD_LOCAL.get().getTime();
            long endTime = System.currentTimeMillis();
            //请求耗时
            Long usedTime = endTime - beginTime;
            systemLogVO.setCostTime(usedTime.intValue());
            //调用线程保存
            ThreadPoolUtil.getPool().execute(new SaveSystemLogThread(systemLogVO, systemLogService));


            BEGIN_TIME_THREAD_LOCAL.remove();
        } catch (Exception e) {
            log.error("系统日志保存异常", e);
        }
    }

    /**
     * 保存日志
     */
    private static class SaveSystemLogThread implements Runnable {
        @Autowired
        private SystemLogVO systemLogVO;
        @Autowired
        private SystemLogService systemLogService;

        public SaveSystemLogThread(SystemLogVO systemLogVO, SystemLogService systemLogService) {
            this.systemLogVO = systemLogVO;
            this.systemLogService = systemLogService;
        }

        @Override
        public void run() {
            try {
                systemLogService.saveLog(systemLogVO);
            } catch (Exception e) {
                log.error("系统日志保存异常,内容{}：", systemLogVO, e);
            }
        }
    }


    /**
     * 获取注解中对方法的描述信息 用于Controller层注解
     *
     * @param joinPoint 切点
     * @return 方法描述
     * @throws Exception
     */
    private static Map<String, String> spelFormat(JoinPoint joinPoint, Object rvt) {

        Map<String, String> result = new HashMap<>(2);
        MethodSignature signature = (MethodSignature) joinPoint.getSignature();
        SystemLogPoint systemLogPoint = signature.getMethod().getAnnotation(SystemLogPoint.class);
        String description = systemLogPoint.description();
        String customerLog = SpelUtil.compileParams(joinPoint, rvt, systemLogPoint.customerLog());

        result.put("description", description);
        result.put("customerLog", customerLog);
        return result;
    }

}
