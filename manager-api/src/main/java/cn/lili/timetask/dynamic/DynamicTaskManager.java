package cn.lili.timetask.dynamic;

import cn.hutool.core.util.StrUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.system.entity.dos.TimedTask;
import cn.lili.modules.system.service.TimedTaskService;
import cn.lili.timetask.dto.TimedTaskUpdateDTO;
import cn.lili.timetask.vo.TimedTaskVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.EventListener;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.scheduling.support.CronExpression;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.stereotype.Component;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

/**
 * 动态定时任务调度管理器（纯数据库表驱动）
 * 完全从数据库 li_timed_task 表加载任务配置，通过 Bean 与方法名反射动态挂载调度
 *
 * @author Bulbasaur
 * @since 2026-10-29
 */
@Slf4j
@Component
public class DynamicTaskManager {

    @Autowired
    @Qualifier("dynamicThreadPoolTaskScheduler")
    private ThreadPoolTaskScheduler taskScheduler;

    @Autowired
    private TimedTaskService timedTaskService;

    @Autowired
    private ApplicationContext applicationContext;

    /**
     * 任务执行体缓存 (key: taskId)
     */
    private final Map<String, Runnable> taskRunnableMap = new ConcurrentHashMap<>();

    /**
     * 正在运行的任务调度 Future (key: taskId)
     */
    private final Map<String, ScheduledFuture<?>> futureMap = new ConcurrentHashMap<>();

    /**
     * 任务执行防重锁
     */
    private final Map<String, AtomicBoolean> runningMap = new ConcurrentHashMap<>();

    /**
     * 容器就绪后，直接从数据库 li_timed_task 表加载任务并挂载调度
     */
    @EventListener(ApplicationReadyEvent.class)
    public synchronized void init() {
        log.debug("开始从数据库 li_timed_task 表加载定时任务...");
        try {
            List<TimedTask> taskList = timedTaskService.list();
            if (taskList == null || taskList.isEmpty()) {
                log.warn("定时任务表 [li_timed_task] 为空，请先执行 SQL 初始化任务数据");
                return;
            }

            for (TimedTask task : taskList) {
                Runnable runnable = resolveRunnable(task.getBeanName(), task.getMethodName());
                if (runnable != null) {
                    taskRunnableMap.put(task.getId(), runnable);
                    if (Boolean.TRUE.equals(task.getStatus())) {
                        scheduleTask(task.getId(), task.getCron());
                    }
                } else {
                    log.error("定时任务 [{}] 解析执行体失败: beanName={}, methodName={}",
                            task.getId(), task.getBeanName(), task.getMethodName());
                }
            }

            log.debug("动态定时任务加载完成，数据库共 {} 个任务，已启动 {} 个任务调度",
                    taskList.size(), futureMap.size());
        } catch (Exception e) {
            log.error("初始化定时任务失败，请检查数据库表 li_timed_task 是否已正确创建", e);
        }
    }

    /**
     * 根据 beanName 与 methodName 反射解析可执行体
     */
    private Runnable resolveRunnable(String beanName, String methodName) {
        if (StrUtil.isBlank(beanName)) {
            return null;
        }
        String finalMethodName = StrUtil.isNotBlank(methodName) ? methodName.trim() : "execute";

        Object bean = findBean(beanName.trim());
        if (bean == null) {
            log.warn("Spring 容器中未找到对应的 Bean: {}", beanName);
            return null;
        }

        try {
            Class<?> targetClass = AopUtils.getTargetClass(bean);
            Method method = targetClass.getMethod(finalMethodName);
            method.setAccessible(true);
            return () -> {
                try {
                    method.invoke(bean);
                } catch (InvocationTargetException ite) {
                    Throwable target = ite.getTargetException() != null ? ite.getTargetException() : ite;
                    throw new RuntimeException(target.getMessage(), target);
                } catch (Exception e) {
                    throw new RuntimeException(e.getMessage(), e);
                }
            };
        } catch (NoSuchMethodException e) {
            log.error("类 [{}] 中未找到无参方法 [{}]", bean.getClass().getName(), finalMethodName);
            return null;
        }
    }

    /**
     * 查找 Spring 容器中的 Bean（支持名称、首字母小写名称、类名查找）
     */
    private Object findBean(String name) {
        if (applicationContext.containsBean(name)) {
            return applicationContext.getBean(name);
        }
        String lowerFirst = StrUtil.lowerFirst(name);
        if (applicationContext.containsBean(lowerFirst)) {
            return applicationContext.getBean(lowerFirst);
        }
        // 尝试按类型简单类名模糊查找
        Map<String, Object> allBeans = applicationContext.getBeansOfType(Object.class);
        for (Object bean : allBeans.values()) {
            if (AopUtils.getTargetClass(bean).getSimpleName().equalsIgnoreCase(name)) {
                return bean;
            }
        }
        return null;
    }

    /**
     * 挂载单个任务调度
     */
    private synchronized boolean scheduleTask(String taskId, String cron) {
        cancelScheduledFuture(taskId);

        Runnable runnable = taskRunnableMap.get(taskId);
        if (runnable == null) {
            log.warn("无法启动调度，任务 [{}] 尚未解析到可执行代码", taskId);
            return false;
        }

        try {
            if (!CronExpression.isValidExpression(cron)) {
                log.error("任务 [{}] 的 Cron 表达式无效: {}", taskId, cron);
                return false;
            }

            ScheduledFuture<?> future = taskScheduler.schedule(
                    () -> executeTask(taskId, false),
                    new CronTrigger(cron)
            );
            futureMap.put(taskId, future);
            log.debug("定时任务 [{}] 注册调度成功，Cron: {}", taskId, cron);
            return true;
        } catch (Exception e) {
            log.error("注册任务调度失败 [{}] - {}", taskId, cron, e);
            return false;
        }
    }

    /**
     * 取消已有调度
     */
    private synchronized void cancelScheduledFuture(String taskId) {
        ScheduledFuture<?> future = futureMap.remove(taskId);
        if (future != null && !future.isCancelled()) {
            future.cancel(false);
            log.debug("定时任务 [{}] 调度已停止", taskId);
        }
    }

    /**
     * 统一执行任务入口（包含计时、防重、状态记录、入库）
     */
    public void executeTask(String taskId, boolean isManual) {
        TimedTask task = timedTaskService.getById(taskId);
        if (task == null) {
            throw new ServiceException(ResultCode.ERROR, "任务不存在: " + taskId);
        }

        Runnable runnable = taskRunnableMap.get(taskId);
        if (runnable == null) {
            // 尝试懒加载解析一次
            runnable = resolveRunnable(task.getBeanName(), task.getMethodName());
            if (runnable != null) {
                taskRunnableMap.put(taskId, runnable);
            } else {
                throw new ServiceException(ResultCode.ERROR, "未找到任务执行体: " + taskId);
            }
        }

        AtomicBoolean running = runningMap.computeIfAbsent(taskId, k -> new AtomicBoolean(false));
        if (!running.compareAndSet(false, true)) {
            log.warn("任务 [{}] 正在执行中，本次执行跳过 (是否手动: {})", taskId, isManual);
            if (isManual) {
                throw new ServiceException(ResultCode.ERROR, "任务正在执行中，请勿重复触发");
            }
            return;
        }

        long startTime = System.currentTimeMillis();
        log.info("开始执行定时任务 [{} - {}]，触发方式: {}", task.getId(), task.getTaskName(), isManual ? "手动触发" : "定时调度");
        boolean success = true;
        String errorMessage = null;
        try {
            runnable.run();
            log.info("定时任务 [{} - {}] 执行成功，耗时 {} ms", task.getId(), task.getTaskName(), System.currentTimeMillis() - startTime);
        } catch (Throwable e) {
            success = false;
            errorMessage = e.getMessage() != null ? e.getMessage() : e.toString();
            log.error("定时任务 [{} - {}] 执行失败: {}", task.getId(), task.getTaskName(), errorMessage, e);
            if (isManual) {
                throw new ServiceException(ResultCode.ERROR, "任务执行失败: " + errorMessage);
            }
        } finally {
            long cost = System.currentTimeMillis() - startTime;
            running.set(false);
            updateTaskExecutionStatus(taskId, success, cost, errorMessage);
        }
    }

    /**
     * 更新数据库执行状态
     */
    private void updateTaskExecutionStatus(String taskId, boolean success, long cost, String errorMessage) {
        try {
            TimedTask update = new TimedTask();
            update.setId(taskId);
            update.setLastExecTime(new Date());
            update.setLastExecStatus(success ? 1 : 0);
            update.setLastExecCost(cost);
            update.setLastExecError(errorMessage);
            timedTaskService.updateById(update);
        } catch (Exception e) {
            log.error("更新定时任务 [{}] 执行记录入库失败", taskId, e);
        }
    }

    /**
     * 获取数据库所有任务列表
     */
    public List<TimedTaskVO> getTaskList() {
        List<TimedTask> list = timedTaskService.list();
        if (list == null) {
            return Collections.emptyList();
        }
        return list.stream().map(this::toVO).collect(Collectors.toList());
    }

    /**
     * 修改任务（Cron 表达式、启用状态）
     */
    public synchronized void updateTask(TimedTaskUpdateDTO dto) {
        TimedTask task = timedTaskService.getById(dto.getId());
        if (task == null) {
            throw new ServiceException(ResultCode.ERROR, "未知任务ID: " + dto.getId());
        }

        if (StrUtil.isNotBlank(dto.getCron())) {
            String newCron = dto.getCron().trim();
            if (!CronExpression.isValidExpression(newCron)) {
                throw new ServiceException(ResultCode.ERROR, "Cron表达式无效: " + newCron);
            }
            task.setCron(newCron);
        }

        if (dto.getStatus() != null) {
            task.setStatus(dto.getStatus());
        }

        timedTaskService.updateById(task);

        // 重新挂载或停止调度
        if (Boolean.TRUE.equals(task.getStatus())) {
            scheduleTask(task.getId(), task.getCron());
        } else {
            cancelScheduledFuture(task.getId());
        }
    }

    /**
     * 手动触发指定任务
     */
    public void triggerTask(String taskId) {
        executeTask(taskId, true);
    }

    /**
     * 注册任务执行体（主要供单元测试直接注入 Mock 执行体）
     */
    public void registerTaskRunnableForTest(String taskId, Runnable runnable) {
        taskRunnableMap.put(taskId, runnable);
        runningMap.put(taskId, new AtomicBoolean(false));
    }

    private TimedTaskVO toVO(TimedTask task) {
        return TimedTaskVO.builder()
                .id(task.getId())
                .name(task.getTaskName())
                .group(task.getGroupName())
                .description(task.getDescription())
                .beanName(task.getBeanName())
                .methodName(task.getMethodName())
                .cron(task.getCron())
                .status(task.getStatus())
                .lastExecTime(task.getLastExecTime())
                .lastExecStatus(task.getLastExecStatus())
                .lastExecCost(task.getLastExecCost())
                .lastExecError(task.getLastExecError())
                .build();
    }
}
