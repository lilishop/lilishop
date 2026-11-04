package cn.lili.timetask.dynamic;

import cn.lili.common.exception.ServiceException;
import cn.lili.modules.system.entity.dos.TimedTask;
import cn.lili.modules.system.service.TimedTaskService;
import cn.lili.timetask.dto.TimedTaskUpdateDTO;
import cn.lili.timetask.vo.TimedTaskVO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationContext;
import org.springframework.scheduling.concurrent.ThreadPoolTaskScheduler;
import org.springframework.scheduling.support.CronTrigger;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * 动态定时任务管理器单元测试
 *
 * @author Bulbasaur
 * @since 2026-10-29
 */
@ExtendWith(MockitoExtension.class)
class DynamicTaskManagerTest {

    @Mock
    private ThreadPoolTaskScheduler taskScheduler;

    @Mock
    private TimedTaskService timedTaskService;

    @Mock
    private ApplicationContext applicationContext;

    @InjectMocks
    private DynamicTaskManager dynamicTaskManager;

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        ReflectionTestUtils.setField(dynamicTaskManager, "taskScheduler", taskScheduler);
        ReflectionTestUtils.setField(dynamicTaskManager, "timedTaskService", timedTaskService);
        ReflectionTestUtils.setField(dynamicTaskManager, "applicationContext", applicationContext);
    }

    /**
     * 验证动态任务列表查询结果。
     */
    @Test
    void testGetTaskList() {
        TimedTask task = TimedTask.builder()
                .id("TEST_TASK")
                .taskName("测试任务")
                .groupName("测试分组")
                .cron("0 0/5 * * * ?")
                .status(true)
                .build();
        when(timedTaskService.list()).thenReturn(Collections.singletonList(task));

        List<TimedTaskVO> list = dynamicTaskManager.getTaskList();
        assertNotNull(list);
        assertEquals(1, list.size());
        assertEquals("TEST_TASK", list.get(0).getId());
        assertEquals("测试任务", list.get(0).getName());
    }

    /**
     * 验证手动触发指定动态任务。
     */
    @Test
    void testTriggerTask() {
        TimedTask task = TimedTask.builder()
                .id("TEST_TASK")
                .taskName("测试任务")
                .cron("0 0/5 * * * ?")
                .status(true)
                .build();
        when(timedTaskService.getById("TEST_TASK")).thenReturn(task);

        AtomicInteger count = new AtomicInteger(0);
        dynamicTaskManager.registerTaskRunnableForTest("TEST_TASK", count::incrementAndGet);

        dynamicTaskManager.triggerTask("TEST_TASK");
        assertEquals(1, count.get());
        verify(timedTaskService, atLeastOnce()).updateById(any(TimedTask.class));
    }

    /**
     * 验证修改动态任务时拒绝非法Cron表达式。
     */
    @Test
    void testUpdateTaskInvalidCron() {
        TimedTask task = TimedTask.builder()
                .id("TEST_TASK")
                .taskName("测试任务")
                .cron("0 0/5 * * * ?")
                .status(true)
                .build();
        when(timedTaskService.getById("TEST_TASK")).thenReturn(task);

        TimedTaskUpdateDTO dto = new TimedTaskUpdateDTO();
        dto.setId("TEST_TASK");
        dto.setCron("invalid cron");

        assertThrows(ServiceException.class, () -> dynamicTaskManager.updateTask(dto));
    }

    /**
     * 验证修改动态任务时接受合法Cron表达式。
     */
    @Test
    void testUpdateTaskValidCron() {
        TimedTask task = TimedTask.builder()
                .id("TEST_TASK")
                .taskName("测试任务")
                .cron("0 0/5 * * * ?")
                .status(true)
                .build();
        when(timedTaskService.getById("TEST_TASK")).thenReturn(task);

        dynamicTaskManager.registerTaskRunnableForTest("TEST_TASK", () -> {});

        TimedTaskUpdateDTO dto = new TimedTaskUpdateDTO();
        dto.setId("TEST_TASK");
        dto.setCron("0 0 12 * * ?");
        dto.setStatus(true);

        dynamicTaskManager.updateTask(dto);

        verify(timedTaskService, atLeastOnce()).updateById(task);
        verify(taskScheduler, atLeastOnce()).schedule(any(Runnable.class), any(CronTrigger.class));
    }
}
