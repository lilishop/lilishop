package cn.lili.modules.order.order.aop;

import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.SpelUtil;
import cn.lili.common.utils.ThreadPoolUtil;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.After;
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
public class OrderOperationLogAspect {

    @Autowired
    private OrderLogService orderLogService;

    @After("@annotation(cn.lili.modules.order.order.aop.OrderLogPoint)")
    public void doAfter(JoinPoint joinPoint) {
        try {

            //日志对象拼接
            //默认操作人员，系统操作
            String userName = "系统操作", id = "-1", role = UserEnums.SYSTEM.getRole();
            if (UserContext.getCurrentUser() != null) {
                //日志对象拼接
                userName = UserContext.getCurrentUser().getUsername();
                id = UserContext.getCurrentUser().getId();
                role = UserContext.getCurrentUser().getRole().getRole();
            }
            Map<String, String> orderLogPoints = spelFormat(joinPoint);
            OrderLog orderLog = new OrderLog(orderLogPoints.get("orderSn"), id, role, userName, orderLogPoints.get("description"));
            //调用线程保存
            ThreadPoolUtil.getPool().execute(new SaveOrderLogThread(orderLog, orderLogService));
        } catch (Exception e) {
            log.error("订单日志错误",e);
        }
    }

    /**
     * 获取注解中对方法的描述信息 用于Controller层注解
     *
     * @param joinPoint 切点
     * @return 方法描述
     * @throws Exception
     */
    private static Map<String, String> spelFormat(JoinPoint joinPoint) throws Exception {

        Map<String, String> result = new HashMap<>(2);
        MethodSignature signature = (MethodSignature) joinPoint.getSignature();
        OrderLogPoint orderLogPoint = signature.getMethod().getAnnotation(OrderLogPoint.class);
        String description = SpelUtil.compileParams(joinPoint, orderLogPoint.description());
        String orderSn = SpelUtil.compileParams(joinPoint, orderLogPoint.orderSn());

        result.put("description", description);
        result.put("orderSn", orderSn);
        return result;
    }

    /**
     * 保存日志
     */
    private static class SaveOrderLogThread implements Runnable {

        private final OrderLog orderLog;
        private final OrderLogService orderLogService;

        public SaveOrderLogThread(OrderLog orderLog, OrderLogService orderLogService) {
            this.orderLog = orderLog;
            this.orderLogService = orderLogService;
        }

        @Override
        public void run() {
            orderLogService.save(orderLog);
        }
    }

}
