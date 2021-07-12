package cn.lili.common.utils;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.DefaultTypedTuple;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * Redis封装工具类
 *
 * @author paulG
 * @since 2020/11/7
 **/
@Slf4j
@Component
public class RedisUtil {
    @Autowired
    private RedisTemplate redisTemplate;

    //=============================common============================

    /**
     * 指定缓存失效时间
     *
     * @param key  键
     * @param time 时间(秒)
     * @return
     */
    public boolean expire(String key, long time) {
        try {
            if (time > 0) {
                redisTemplate.expire(key, time, TimeUnit.SECONDS);
            }
            return true;
        } catch (Exception e) {
            log.error("指定缓存失效时间错误",e);
            return false;
        }
    }


    //============================String=============================

    /**
     * 普通缓存获取
     *
     * @param key 键
     * @return 值
     */
    public Object get(String key) {
        return key == null ? null : redisTemplate.opsForValue().get(key);
    }


    /**
     * 普通缓存获取
     *
     * @param key 键
     * @return 值
     */
    public <T> T get(String key, Class<T> clazz) {
        Object o = key == null ? null : redisTemplate.opsForValue().get(key);
        return (T) o;
    }

    /**
     * 普通缓存放入
     *
     * @param key   键
     * @param value 值
     * @return true成功 false失败
     */
    public boolean set(String key, Object value) {
        try {
            redisTemplate.opsForValue().set(key, value);
            return true;
        } catch (Exception e) {
            log.error("缓存放入错误",e);
            return false;
        }

    }

    /**
     * 普通缓存放入并设置时间
     *
     * @param key   键
     * @param value 值
     * @param time  时间(秒) time要大于0 如果time小于等于0 将设置无限期
     * @return true成功 false 失败
     */
    public boolean set(String key, Object value, long time) {
        try {
            if (time > 0) {
                redisTemplate.opsForValue().set(key, value, time, TimeUnit.SECONDS);
            } else {
                set(key, value);
            }
            return true;
        } catch (Exception e) {
            log.error("普通缓存放入并设置时间错误",e);
            return false;
        }
    }

    //================================Map=================================


    /**
     * 将数据放入set缓存
     *
     * @param key    键
     * @param values 值 可以是多个
     * @return 成功个数
     */
    public long sSet(String key, Object... values) {
        try {
            return redisTemplate.opsForSet().add(key, values);
        } catch (Exception e) {
            log.error("将数据放入set缓存错误",e);
            return 0;
        }
    }


    /**
     * 将list放入缓存
     *
     * @param key   键
     * @param value 值
     * @return
     */
    public boolean lSet(String key, Object value) {
        try {
            redisTemplate.opsForList().rightPush(key, value);
            return true;
        } catch (Exception e) {
            log.error("将list放入缓存错误",e);
            return false;
        }
    }

    /**
     * 将list放入缓存
     *
     * @param key   键
     * @param value 值
     * @param time  时间(秒)
     * @return
     */
    public boolean lSet(String key, Object value, long time) {
        try {
            redisTemplate.opsForList().rightPush(key, value);
            if (time > 0) {
                expire(key, time);
            }
            return true;
        } catch (Exception e) {
            log.error("将list放入缓存错误",e);
            return false;
        }
    }

    /**
     * 将list放入缓存
     *
     * @param key   键
     * @param value 值
     * @return
     */
    public boolean lSet(String key, List<Object> value) {
        try {
            redisTemplate.opsForList().rightPushAll(key, value);
            return true;
        } catch (Exception e) {
            log.error("将list放入缓存错误",e);
            return false;
        }
    }


    /**
     * 将list放入缓存
     *
     * @param key   键
     * @param value 值
     * @param time  时间(秒)
     * @return
     */
    public boolean lSet(String key, List<Object> value, long time) {
        try {
            redisTemplate.opsForList().rightPushAll(key, value);
            if (time > 0) {
                expire(key, time);
            }
            return true;
        } catch (Exception e) {
            log.error("将list放入缓存错误",e);
            return false;
        }
    }


    //===============================ZSet=================================

    /**
     * 向Zset里添加成员
     *
     * @param key
     * @param score
     * @param value
     * @return
     */
    public boolean zadd(String key, long score, String value) {
        return redisTemplate.opsForZSet().add(key, value, score);

    }


    /**
     * 获取 某key 下 某一分值区间的队列
     *
     * @param key
     * @param from
     * @param to
     * @return
     */
    public Set<DefaultTypedTuple> zrangeByScoreWithScores(String key, int from, long to) {
        Set<DefaultTypedTuple> set = redisTemplate.opsForZSet().rangeByScoreWithScores(key, from, to);
        return set;
    }

    /**
     * 移除 Zset队列值
     *
     * @param key
     * @param value
     * @return
     */
    public Long zremove(String key, String... value) {
        return redisTemplate.opsForZSet().remove(key, value);

    }

}
