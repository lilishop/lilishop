package cn.lili.cache.impl;

import cn.lili.cache.Cache;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.connection.RedisConnection;
import org.springframework.data.redis.core.*;
import org.springframework.data.redis.support.atomic.RedisAtomicLong;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

/**
 * redis 缓存实现
 *
 * @author Chopepr
 */
@Slf4j
@Component
public class RedisCache implements Cache {

    @Autowired
    private RedisTemplate<Object, Object> redisTemplate;

    public RedisCache() {

    }

    @Override
    public Object get(Object key) {

        return redisTemplate.opsForValue().get(key);
    }

    @Override
    public String getString(Object key) {
        try {
            return redisTemplate.opsForValue().get(key).toString();
        } catch (Exception e) {
            return null;
        }
    }

    @Override
    public List multiGet(Collection keys) {
        return redisTemplate.opsForValue().multiGet(keys);

    }


    @Override
    public void multiSet(Map map) {
        redisTemplate.opsForValue().multiSet(map);
    }

    @Override
    public void multiDel(Collection keys) {
        redisTemplate.delete(keys);
    }

    @Override
    public void put(Object key, Object value) {
        redisTemplate.opsForValue().set(key, value);
    }

    @Override
    public void put(Object key, Object value, Long exp) {
        put(key, value, exp, TimeUnit.SECONDS);
    }

    @Override
    public void put(Object key, Object value, Long exp, TimeUnit timeUnit) {
        redisTemplate.opsForValue().set(key, value, exp, timeUnit);
    }

    @Override
    public Boolean remove(Object key) {

        return redisTemplate.delete(key);
    }

    /**
     * 删除
     *
     * @param key 模糊删除key
     */
    @Override
    public void vagueDel(Object key) {
        List keys = this.keys(key + "*");
        redisTemplate.delete(keys);
    }

    @Override
    public void clear() {
        List keys = this.keys("*");
        redisTemplate.delete(keys);
    }

    @Override
    public void putHash(Object key, Object hashKey, Object hashValue) {
        redisTemplate.opsForHash().put(key, hashKey, hashValue);
    }

    @Override
    public void putAllHash(Object key, Map map) {
        redisTemplate.opsForHash().putAll(key, map);
    }

    @Override
    public Object getHash(Object key, Object hashKey) {
        return redisTemplate.opsForHash().get(key, hashKey);
    }

    @Override
    public Map<Object, Object> getHash(Object key) {
        return this.redisTemplate.opsForHash().entries(key);
    }

    @Override
    public boolean hasKey(Object key) {
        return this.redisTemplate.opsForValue().get(key) != null;
    }

    /**
     * 获取符合条件的key
     *
     * @param pattern 表达式
     * @return 模糊匹配key
     */
    @Override
    public List<Object> keys(String pattern) {
        List<Object> keys = new ArrayList<>();
        this.scan(pattern, item -> {
            //符合条件的key
            String key = new String(item, StandardCharsets.UTF_8);
            keys.add(key);
        });
        return keys;
    }

    @Override
    public List<Object> keysBlock(String pattern) {
        Set<Object> set = redisTemplate.keys(pattern);
        List<Object> list = new ArrayList<>();
        list.addAll(set);
        return list;
    }

    /**
     * scan 实现
     *
     * @param pattern  表达式
     * @param consumer 对迭代到的key进行操作
     */
    private void scan(String pattern, Consumer<byte[]> consumer) {
        this.redisTemplate.execute((RedisConnection connection) -> {
            try (Cursor<byte[]> cursor =
                         connection.scan(ScanOptions.scanOptions()
                                 .count(Long.MAX_VALUE)
                                 .match(pattern).build())) {
                cursor.forEachRemaining(consumer);
                return null;

            } catch (IOException e) {
                log.error("scan错误", e);
                throw new RuntimeException(e);
            }
        });
    }


    @Override
    public Long cumulative(Object key, Object value) {
        HyperLogLogOperations<Object, Object> operations = redisTemplate.opsForHyperLogLog();
        //add 方法对应 PFADD 命令
        return operations.add(key, value);

    }

    @Override
    public Long counter(Object key) {
        HyperLogLogOperations<Object, Object> operations = redisTemplate.opsForHyperLogLog();

        //add 方法对应 PFADD 命令
        return operations.size(key);
    }

    @Override
    public List multiCounter(Collection keys) {
        if (keys == null) {
            return new ArrayList();
        }
        List<Long> result = new ArrayList<>();
        for (Object key : keys) {
            result.add(counter(key));
        }
        return result;
    }

    @Override
    public Long mergeCounter(Object... key) {
        HyperLogLogOperations<Object, Object> operations = redisTemplate.opsForHyperLogLog();
        //计数器合并累加
        return operations.union(key[0], key);
    }

    @Override
    public Long incr(String key, long liveTime) {
        RedisAtomicLong entityIdCounter = new RedisAtomicLong(key, redisTemplate.getConnectionFactory());
        Long increment = entityIdCounter.getAndIncrement();
        //初始设置过期时间
        if (increment == 0 && liveTime > 0) {
            entityIdCounter.expire(liveTime, TimeUnit.SECONDS);
        }

        return increment;
    }

    @Override
    public Long incr(String key) {
        RedisAtomicLong entityIdCounter = new RedisAtomicLong(key, redisTemplate.getConnectionFactory());
        return entityIdCounter.getAndIncrement();
    }

    /**
     * 使用Sorted Set记录keyword
     * zincrby命令，对于一个Sorted Set，存在的就把分数加x(x可自行设定)，不存在就创建一个分数为1的成员
     *
     * @param sortedSetName sortedSetName的Sorted Set不用预先创建，不存在会自动创建，存在则向里添加数据
     * @param keyword       关键词
     */
    @Override
    public void incrementScore(String sortedSetName, String keyword) {
        //指向key名为KEY的zset元素
        redisTemplate.opsForZSet().incrementScore(sortedSetName, keyword, 1);
    }

    @Override
    public void incrementScore(String sortedSetName, String keyword, Integer score) {
        redisTemplate.opsForZSet().incrementScore(sortedSetName, keyword, score);
    }

    /**
     * zrevrange命令, 查询Sorted Set中指定范围的值
     * 返回的有序集合中，score大的在前面
     * zrevrange方法无需担心用于指定范围的start和end出现越界报错问题
     *
     * @param sortedSetName sortedSetName
     * @param start         查询范围开始位置
     * @param end           查询范围结束位置
     * @return 符合排序的集合
     */
    @Override
    public Set<ZSetOperations.TypedTuple<Object>> reverseRangeWithScores(String sortedSetName, Integer start, Integer end) {
        return this.redisTemplate.opsForZSet().reverseRangeWithScores(sortedSetName, start, end);
    }

    /**
     * zrevrange命令, 查询Sorted Set中指定范围的值
     * 返回的有序集合中，score大的在前面
     * zrevrange方法无需担心用于指定范围的start和end出现越界报错问题
     *
     * @param sortedSetName sortedSetName
     * @param count         获取数量
     * @return 符合排序的集合
     */
    @Override
    public Set<ZSetOperations.TypedTuple<Object>> reverseRangeWithScores(String sortedSetName, Integer count) {
        return this.redisTemplate.opsForZSet().reverseRangeWithScores(sortedSetName, 0, count);
    }


    /**
     * 向Zset里添加成员
     *
     * @param key   key值
     * @param score 分数，通常用于排序
     * @param value 值
     * @return 增加状态
     */
    @Override
    public boolean zAdd(String key, long score, String value) {
        return redisTemplate.opsForZSet().add(key, value, score);

    }


    /**
     * 获取 某key 下 某一分值区间的队列
     *
     * @param key  缓存key
     * @param from 开始时间
     * @param to   结束时间
     * @return 数据
     */
    @Override
    public Set<ZSetOperations.TypedTuple<Object>> zRangeByScore(String key, int from, long to) {
        Set<ZSetOperations.TypedTuple<Object>> set = redisTemplate.opsForZSet().rangeByScoreWithScores(key, from, to);
        return set;
    }

    /**
     * 移除 Zset队列值
     *
     * @param key   key值
     * @param value 删除的集合
     * @return 删除数量
     */
    @Override
    public Long zRemove(String key, String... value) {
        return redisTemplate.opsForZSet().remove(key, value);
    }
}
