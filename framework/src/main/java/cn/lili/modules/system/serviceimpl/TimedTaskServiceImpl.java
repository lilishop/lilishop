package cn.lili.modules.system.serviceimpl;

import cn.lili.modules.system.entity.dos.TimedTask;
import cn.lili.modules.system.mapper.TimedTaskMapper;
import cn.lili.modules.system.service.TimedTaskService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * 定时任务业务层实现
 *
 * @author Bulbasaur
 * @since 2026-10-29
 */
@Service
public class TimedTaskServiceImpl extends ServiceImpl<TimedTaskMapper, TimedTask> implements TimedTaskService {

}
