package cn.lili.timetask.handler.impl.appointment;

import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.timetask.handler.EveryMinuteExecute;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 预约订单每分钟定时任务：逾期未核销自动完结、服务开始前提醒
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Slf4j
@Component
public class AppointmentMinuteTaskExecute implements EveryMinuteExecute {

    @Autowired
    private AppointmentService appointmentService;

    @Override
    public void execute() {
        try {
            appointmentService.processNoShowAutoComplete();
        } catch (Exception e) {
            log.error("预约逾期未核销自动完结任务异常", e);
        }
        try {
            appointmentService.processServiceStartRemind();
        } catch (Exception e) {
            log.error("预约服务开始前提醒任务异常", e);
        }
    }
}
