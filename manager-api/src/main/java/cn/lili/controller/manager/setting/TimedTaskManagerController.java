package cn.lili.controller.manager.setting;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.timetask.dto.TimedTaskUpdateDTO;
import cn.lili.timetask.dynamic.DynamicTaskManager;
import cn.lili.timetask.vo.TimedTaskVO;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 管理端,定时任务管理接口
 */
@Slf4j
@RestController
@Tag(name = "管理端,定时任务管理接口")
@RequestMapping("/manager/setting/timed-task")
public class TimedTaskManagerController {

    @Autowired
    private DynamicTaskManager dynamicTaskManager;

    @Operation(summary = "获取定时任务列表")
    @GetMapping
    public ResultMessage<List<TimedTaskVO>> list() {
        return ResultUtil.data(dynamicTaskManager.getTaskList());
    }

    @Operation(summary = "修改定时任务配置（Cron表达式、启用/暂停状态）")
    @PutMapping
    public ResultMessage<Object> update(@Validated @RequestBody TimedTaskUpdateDTO dto) {
        log.info("修改定时任务: {}", dto);
        dynamicTaskManager.updateTask(dto);
        return ResultUtil.success();
    }

    @Operation(summary = "手动触发指定任务")
    @PostMapping("/trigger/{taskId}")
    public ResultMessage<Object> trigger(@PathVariable String taskId) {
        log.info("手动触发任务: {}", taskId);
        dynamicTaskManager.triggerTask(taskId);
        return ResultUtil.success();
    }
}
