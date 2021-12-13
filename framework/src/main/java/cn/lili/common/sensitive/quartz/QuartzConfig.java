package cn.lili.common.sensitive.quartz;

import org.quartz.*;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * 定时执行配置
 *
 * @author Chopper
 * @version v1.0
 * 2021-11-23 16:30
 */
@Configuration
public class QuartzConfig {

    @Bean
    public JobDetail sensitiveQuartzDetail() {
        return JobBuilder.newJob(SensitiveQuartz.class).withIdentity("sensitiveQuartz").storeDurably().build();
    }

    @Bean
    public Trigger sensitiveQuartzTrigger() {
        SimpleScheduleBuilder scheduleBuilder = SimpleScheduleBuilder.simpleSchedule()
                .withIntervalInSeconds(3600)
                .repeatForever();
        return TriggerBuilder.newTrigger().forJob(sensitiveQuartzDetail())
                .withIdentity("sensitiveQuartz")
                .withSchedule(scheduleBuilder)
                .build();
    }
}