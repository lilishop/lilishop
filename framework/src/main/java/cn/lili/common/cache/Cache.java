package cn.lili.common.cache;


import org.springframework.data.redis.core.ZSetOperations;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/**
 * 缓存接口
 *
 * @author Chopper
 */
public interface Cache<T> {

    /**
     * Get an item from the cache, nontransactionally
     *
     * @param key
     * @return the cached object or <tt>null</tt>
     */
    T get(Object key);

    /**
     * Get an item from the cache, nontransactionally
     *
     * @param key
     * @return the cached object or <tt>null</tt>
     */
    String getString(Object key);


    /**
     * multiGet
     *
     * @param keys 要查询的key集合
     * @return
     */
    List multiGet(Collection keys);

    /**
     * 批量set
     *
     * @param map
     */
    void multiSet(Map map);


    /**
     * 批量删除
     *
     * @param keys 要删除的key集合
     */
    void multiDel(Collection keys);

    /**
     * Add an item to the cache, nontransactionally, with
     * failfast semantics
     *
     * @param key
     * @param value
     */
    void put(Object key, T value);

    /**
     * 往缓存中写入内容
     *
     * @param key
     * @param value
     * @param exp   超时时间，单位为秒
     */
    void put(Object key, T value, Long exp);

    /**
     * 往缓存中写入内容
     *
     * @param key
     * @param value
     * @param exp
     * @param timeUnit 写入单位
     */
    void put(Object key, T value, Long exp, TimeUnit timeUnit);

    /**
     * 删除
     *
     * @param key
     */
    void remove(Object key);

    /**
     * 删除
     *
     * @param key
     */
    void vagueDel(Object key);

    /**
     * Clear the cache
     */
    void clear();


    /**
     * 往缓存中写入内容
     *
     * @param key       缓存key
     * @param hashKey   缓存中hashKey
     * @param hashValue hash值
     */
    void putHash(Object key, Object hashKey, Object hashValue);

    /**
     * 玩缓存中写入内容
     *
     * @param key
     * @param map
     */
    void putAllHash(Object key, Map map);

    /**
     * 读取缓存值
     *
     * @param key
     * @param hashKey
     * @return
     */
    T getHash(Object key, Object hashKey);

    /**
     * 读取缓存值
     *
     * @param key
     * @return
     */
    Map<Object, Object> getHash(Object key);

    /**
     * 是否包含
     *
     * @param key
     * @return
     */
    boolean hasKey(Object key);


    /**
     * 模糊匹配key
     *
     * @param pattern
     * @return
     */
    List<String> keys(String pattern);


    //-----------------------------------------------用于特殊场景，redis去重计数---------------------------------------------

    /**
     * 累计数目
     * 效率较高的 计数器
     * 如需清零，按照普通key 移除即可
     *
     * @param key
     * @param value
     * @return
     */
    Long cumulative(Object key, Object value);

    /**
     * 计数器结果
     * <p>
     * 效率较高的 计数器 统计返回
     * 如需清零，按照普通key 移除即可
     *
     * @param key
     * @return
     */
    Long counter(Object key);

    /**
     * 批量计数
     *
     * @param keys 要查询的key集合
     * @return
     */
    List multiCounter(Collection keys);

    /**
     * 计数器结果
     * <p>
     * 效率较高的 计数器 统计返回
     * 如需清零，按照普通key 移除即可
     *
     * @param key
     * @return
     */
    Long mergeCounter(Object... key);
    //---------------------------------------------------用于特殊场景，redis去重统计-----------------------------------------

    //-----------------------------------------------redis计数---------------------------------------------

    /**
     * redis 计数器 累加
     * 注：到达liveTime之后，该次增加取消，即自动-1，而不是redis值为空
     *
     * @param key      为累计的key，同一key每次调用则值 +1
     * @param liveTime 单位秒后失效
     * @return
     */
    Long incr(String key, long liveTime);
    //-----------------------------------------------redis计数---------------------------------------------

    /**
     * 使用Sorted Set记录keyword
     * zincrby命令，对于一个Sorted Set，存在的就把分数加x(x可自行设定)，不存在就创建一个分数为1的成员
     *
     * @param sortedSetName sortedSetName的Sorted Set不用预先创建，不存在会自动创建，存在则向里添加数据
     * @param keyword       关键词
     */
    void incrementScore(String sortedSetName, String keyword);

    /**
     * zrevrange命令, 查询Sorted Set中指定范围的值
     * 返回的有序集合中，score大的在前面
     * zrevrange方法无需担心用于指定范围的start和end出现越界报错问题
     *
     * @param sortedSetName sortedSetName
     * @param start         查询范围开始位置
     * @param end           查询范围结束位置
     * @return
     */
    Set<ZSetOperations.TypedTuple<Object>> reverseRangeWithScores(String sortedSetName, Integer start, Integer end);


    /**
     * 向Zset里添加成员
     *
     * @param key   key值
     * @param score 分数
     * @param value 值
     * @return 增加状态
     */
    boolean zAdd(String key, long score, String value);


    /**
     * 获取 某key 下 某一分值区间的队列
     *
     * @param key  缓存key
     * @param from 开始时间
     * @param to   结束时间
     * @return 数据
     */
    Set<ZSetOperations.TypedTuple<Object>> zRangeByScore(String key, int from, long to);

    /**
     * 移除 Zset队列值
     *
     * @param key   key值
     * @param value 删除的集合
     * @return 删除数量
     */
    Long zRemove(String key, String... value);
}
