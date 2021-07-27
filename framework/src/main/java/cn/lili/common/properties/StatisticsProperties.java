package cn.lili.common.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * 在线人数统计
 *
 * @author Chopper
 * @version v4.0
 * @since 2021/2/21 10:19
 */
@Data
@Configuration
@ConfigurationProperties(prefix = "lili.statistics")
public class StatisticsProperties {

    /**
     * 在线人数统计 X 小时
     */
    private Integer onlineMember = 48;

    /**
     * 当前在线人数 刷新时间间隔
     */
    private Integer currentOnlineUpdate = 600;

    public Integer getOnlineMember() {
        if (onlineMember == null) {
            return 48;
        }
        return onlineMember;
    }

    public Integer getCurrentOnlineUpdate() {
        if (currentOnlineUpdate == null) {
            return 600;
        }
        return currentOnlineUpdate;
    }
}
