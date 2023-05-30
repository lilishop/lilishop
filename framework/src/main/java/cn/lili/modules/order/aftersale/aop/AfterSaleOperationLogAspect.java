package cn.lili.modules.order.aftersale.aop;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.SpelUtil;
import cn.lili.common.utils.ThreadPoolUtil;
import cn.lili.modules.order.aftersale.entity.dos.AfterSaleLog;
import cn.lili.modules.order.aftersale.service.AfterSaleLogService;
import cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;

/**
 * 订单操作日志
 *
 * @author Chopper
 * @since 2020/11/17 7:22 下午
 */
@Slf4j
@Aspect
@Component
public class AfterSaleOperationLogAspect {

    @Autowired
    private AfterSaleLogService afterSaleLogService;

    @AfterReturning(returning = "rvt", pointcut = "@annotation(cn.lili.modules.order.aftersale.aop.AfterSaleLogPoint)")
    public void afterReturning(JoinPoint joinPoint, Object rvt) {
        try {
            AuthUser auth = UserContext.getCurrentUser();
            //日志对象拼接
            //默认操作人员，系统操作
            String userName = "系统操作", id = "-1", role = UserEnums.SYSTEM.getRole();
            if (auth != null) {
                //日志对象拼接
                userName = UserContext.getCurrentUser().getUsername();
                id = UserContext.getCurrentUser().getId();
                role = UserContext.getCurrentUser().getRole().getRole();
            }
            Map<String, String> afterSaleLogPoints = spelFormat(joinPoint, rvt);
            AfterSaleLog afterSaleLog = new AfterSaleLog(afterSaleLogPoints.get("sn"), id, role, userName, afterSaleLogPoints.get("description"));
            //调用线程保存
            ThreadPoolUtil.getPool().execute(new SaveAfterSaleLogThread(afterSaleLog, afterSaleLogService));
        } catch (Exception e) {
            log.error("售后日志错误",e);
        }
    }

    /**
     * 获取注解中对方法的描述信息 用于Controller层注解
     *
     * @param joinPoint 切点
     * @return 方法描述
     */
    public static Map<String, String> spelFormat(JoinPoint joinPoint, Object rvt) {

        Map<String, String> result = new HashMap<>(2);
        MethodSignature signature = (MethodSignature) joinPoint.getSignature();
        AfterSaleLogPoint afterSaleLogPoint = signature.getMethod().getAnnotation(AfterSaleLogPoint.class);
        String description = SpelUtil.compileParams(joinPoint, rvt, afterSaleLogPoint.description());
        String sn = SpelUtil.compileParams(joinPoint, rvt, afterSaleLogPoint.sn());
        if (CharSequenceUtil.isNotEmpty(afterSaleLogPoint.serviceStatus())) {
            description += AfterSaleStatusEnum.valueOf(SpelUtil.compileParams(joinPoint, rvt, afterSaleLogPoint.serviceStatus())).description();
        }
        result.put("description", description);
        result.put("sn", sn);
        return result;

    }

    /**
     * 保存日志
     */
    private static class SaveAfterSaleLogThread implements Runnable {

        private final AfterSaleLog afterSaleLog;
        private final AfterSaleLogService afterSaleLogService;

        public SaveAfterSaleLogThread(AfterSaleLog afterSaleLog, AfterSaleLogService afterSaleLogService) {
            this.afterSaleLog = afterSaleLog;
            this.afterSaleLogService = afterSaleLogService;
        }

        @Override
        public void run() {
            afterSaleLogService.save(afterSaleLog);
        }
    }

}
