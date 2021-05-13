package cn.lili.common.utils;

import cn.hutool.core.lang.Snowflake;
import cn.hutool.core.util.IdUtil;

import java.util.Date;

/**
 * 雪花分布式id获取
 *
 * @author Chopper
 */
public class SnowFlake {

    /**
     * 机器id
     */
    private static long workerId = 0L;
    /**
     * 机房id
     */
    private static long datacenterId = 0L;

    private static Snowflake snowflake = IdUtil.createSnowflake(workerId, datacenterId);

    public static long getId() {
        return snowflake.nextId();
    }

    /**
     * 生成字符，带有前缀
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
