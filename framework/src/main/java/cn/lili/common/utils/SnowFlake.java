package cn.lili.common.utils;

import cn.hutool.core.lang.Snowflake;
import cn.hutool.core.util.IdUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.Date;

/**
 * 雪花分布式id获取
 *
 * @author Chopper
 */
@Slf4j
public class SnowFlake {

//    /**
//     * 机器id
//     */
//    private static long workerId = 0L;
//    /**
//     * 机房id
//     */
//    public static long datacenterId = 0L;

    private static Snowflake snowflake;

    /**
     * 初始化配置
     *
     * @param workerId
     * @param datacenterId
     */
    public static void initialize(long workerId, long datacenterId) {
        snowflake = IdUtil.getSnowflake(workerId, datacenterId);
    }

    public static long getId() {
        return snowflake.nextId();
    }

    /**
     * 生成字符，带有前缀
     *
     * @param prefix
     * @return
     */
    public static String createStr(String prefix) {
        return prefix + DateUtil.toString(new Date(), "yyyyMMdd") + SnowFlake.getId();
    }

    public static String getIdStr() {
        return snowflake.nextId() + "";
    }
}
