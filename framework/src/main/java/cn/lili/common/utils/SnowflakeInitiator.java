package cn.lili.common.utils;

import cn.lili.cache.Cache;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;

/**
 * SnowflakeInitiator
 *
 * @author Chopper
 * @version v1.0
 * 2022-01-14 14:04
 */
@Component
@Slf4j
public class SnowflakeInitiator {

    /**
     * 缓存前缀
     */
    private static final String KEY = "{Snowflake}";

    @Autowired
    private Cache cache;

    /**
     * 尝试初始化
     *
     * @return
     */
    @PostConstruct
    public void init() {
        Long num = cache.incr(KEY);
        long dataCenter = num / 32;
        long workedId = num % 32;
        //如果数据中心大于32，则抹除缓存，从头开始
        if (dataCenter >= 32) {
            cache.remove(KEY);
            num = cache.incr(KEY);
            dataCenter = num / 32;
            workedId = num % 32;
        }
        SnowFlake.initialize(workedId, dataCenter);
    }

    public static void main(String[] args) {
        SnowFlake.initialize(0, 8);

        System.out.println(SnowFlake.getId());
    }
}