package cn.lili.it.search;

import cn.lili.cache.Cache;
import cn.lili.modules.goods.entity.dto.RebuildStatus;
import org.springframework.data.redis.core.ZSetOperations;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

/**
 * 仅实现重建进度用到的 get/put；倒排 IT 不启动 Redis。
 *
 * @author Mike
 * @date 2026-09-03
 */
final class ITCache implements Cache<RebuildStatus> {

    private final ConcurrentHashMap<Object, RebuildStatus> store = new ConcurrentHashMap<>();

    /**
     * 从集成测试的内存缓存读取重建进度。
     *
     * @param key 缓存键
     * @return 重建进度
     */
    @Override
    public RebuildStatus get(Object key) {
        return store.get(key);
    }

    /**
     * 读取内存缓存中的重建进度并转换为字符串。
     *
     * @param key 缓存键
     * @return 重建进度字符串，不存在时返回null
     */
    @Override
    public String getString(Object key) {
        RebuildStatus value = store.get(key);
        return value == null ? null : value.toString();
    }

    /**
     * 为未使用的批量缓存读取操作返回空列表。
     *
     * @param keys 缓存键集合
     * @return 空列表
     */
    @Override
    public List multiGet(Collection keys) {
        return List.of();
    }

    /**
     * 拒绝测试替身未实现的批量缓存写入操作。
     *
     * @param map 待写入缓存的数据
     */
    @Override
    public void multiSet(Map map) {
        throw new UnsupportedOperationException();
    }

    /**
     * 拒绝测试替身未实现的批量缓存删除操作。
     *
     * @param keys 缓存键集合
     */
    @Override
    public void multiDel(Collection keys) {
        throw new UnsupportedOperationException();
    }

    /**
     * 将重建进度写入测试内存缓存。
     *
     * @param key   缓存键
     * @param value 测试值
     */
    @Override
    public void put(Object key, RebuildStatus value) {
        store.put(key, value);
    }

    /**
     * 将重建进度写入测试内存缓存，测试替身不处理过期时间。
     *
     * @param key   缓存键
     * @param value 测试值
     * @param exp   缓存过期时长
     */
    @Override
    public void put(Object key, RebuildStatus value, Long exp) {
        store.put(key, value);
    }

    /**
     * 将重建进度写入测试内存缓存，测试替身不处理过期时间。
     *
     * @param key      缓存键
     * @param value    测试值
     * @param exp      缓存过期时长
     * @param timeUnit 时间单位
     */
    @Override
    public void put(Object key, RebuildStatus value, Long exp, TimeUnit timeUnit) {
        store.put(key, value);
    }

    /**
     * 删除指定内存缓存项并返回是否存在该项。
     *
     * @param key 缓存键
     * @return 是否删除了已有缓存项
     */
    @Override
    public Boolean remove(Object key) {
        return store.remove(key) != null;
    }

    /**
     * 按传入键删除测试内存缓存项。
     *
     * @param key 缓存键
     */
    @Override
    public void vagueDel(Object key) {
        store.remove(key);
    }

    /**
     * 清空测试内存缓存。
     */
    @Override
    public void clear() {
        store.clear();
    }

    /**
     * 拒绝测试替身未实现的哈希字段写入操作。
     *
     * @param key       缓存键
     * @param hashKey   哈希字段键
     * @param hashValue 哈希字段值
     */
    @Override
    public void putHash(Object key, Object hashKey, Object hashValue) {
        throw new UnsupportedOperationException();
    }

    /**
     * 拒绝测试替身未实现的哈希批量写入操作。
     *
     * @param key 缓存键
     * @param map 待写入缓存的数据
     */
    @Override
    public void putAllHash(Object key, Map map) {
        throw new UnsupportedOperationException();
    }

    /**
     * 为测试替身未实现的哈希读取操作返回空结果。
     *
     * @param key     缓存键
     * @param hashKey 哈希字段键
     * @return null，测试替身未实现哈希读取
     */
    @Override
    public RebuildStatus getHash(Object key, Object hashKey) {
        return null;
    }

    /**
     * 为测试替身未实现的哈希读取操作返回空结果。
     *
     * @param key 缓存键
     * @return null，测试替身未实现哈希读取
     */
    @Override
    public Map<Object, Object> getHash(Object key) {
        return Map.of();
    }

    /**
     * 判断测试内存缓存是否包含指定键。
     *
     * @param key 缓存键
     * @return 是否包含指定缓存键
     */
    @Override
    public boolean hasKey(Object key) {
        return store.containsKey(key);
    }

    /**
     * 为测试替身未实现的键匹配查询返回空列表。
     *
     * @param pattern 键匹配表达式
     * @return 空列表
     */
    @Override
    public List<Object> keys(String pattern) {
        return List.of();
    }

    /**
     * 为测试替身未实现的阻塞键查询返回空列表。
     *
     * @param pattern 键匹配表达式
     * @return 空列表
     */
    @Override
    public List<Object> keysBlock(String pattern) {
        return List.of();
    }

    /**
     * 为测试替身未实现的累加操作返回零。
     *
     * @param key   缓存键
     * @param value 测试值
     * @return 零，测试替身不执行累加
     */
    @Override
    public Long cumulative(Object key, Object value) {
        return 0L;
    }

    /**
     * 为测试替身未实现的计数读取返回零。
     *
     * @param key 缓存键
     * @return 零，测试替身不保存计数
     */
    @Override
    public Long counter(Object key) {
        return 0L;
    }

    /**
     * 为测试替身未实现的批量计数读取返回空列表。
     *
     * @param keys 缓存键集合
     * @return 空列表
     */
    @Override
    public List multiCounter(Collection keys) {
        return List.of();
    }

    /**
     * 为测试替身未实现的合并计数返回零。
     *
     * @param key 缓存键
     * @return 零，测试替身不执行合并
     */
    @Override
    public Long mergeCounter(Object... key) {
        return 0L;
    }

    /**
     * 为测试替身未实现的递增操作返回零。
     *
     * @param key      缓存键
     * @param liveTime 缓存有效时长
     * @return 零，测试替身不执行递增
     */
    @Override
    public Long incr(String key, long liveTime) {
        return 0L;
    }

    /**
     * 为测试替身未实现的递增操作返回零。
     *
     * @param key 缓存键
     * @return 零，测试替身不执行递增
     */
    @Override
    public Long incr(String key) {
        return 0L;
    }

    /**
     * 拒绝测试替身未实现的有序集合分数更新操作。
     *
     * @param sortedSetName 有序集合名称
     * @param keyword       搜索关键词
     */
    @Override
    public void incrementScore(String sortedSetName, String keyword) {
        throw new UnsupportedOperationException();
    }

    /**
     * 拒绝测试替身未实现的有序集合分数更新操作。
     *
     * @param sortedSetName 有序集合名称
     * @param keyword       搜索关键词
     * @param score         集合成员分数
     */
    @Override
    public void incrementScore(String sortedSetName, String keyword, Integer score) {
        throw new UnsupportedOperationException();
    }

    /**
     * 为测试替身未实现的逆序分数查询返回空集合。
     *
     * @param sortedSetName 有序集合名称
     * @param start         开始位置
     * @param end           结束位置
     * @return 空集合
     */
    @Override
    public Set<ZSetOperations.TypedTuple<Object>> reverseRangeWithScores(String sortedSetName, Integer start, Integer end) {
        return Set.of();
    }

    /**
     * 为测试替身未实现的逆序分数查询返回空集合。
     *
     * @param sortedSetName 有序集合名称
     * @param count         数据数量
     * @return 空集合
     */
    @Override
    public Set<ZSetOperations.TypedTuple<Object>> reverseRangeWithScores(String sortedSetName, Integer count) {
        return Set.of();
    }

    /**
     * 为测试替身未实现的有序集合新增操作返回失败。
     *
     * @param key   缓存键
     * @param score 集合成员分数
     * @param value 测试值
     * @return false，测试替身不执行集合新增
     */
    @Override
    public boolean zAdd(String key, long score, String value) {
        return false;
    }

    /**
     * 为测试替身未实现的分数范围查询返回空集合。
     *
     * @param key  缓存键
     * @param from 起始值
     * @param to   结束值
     * @return 空集合
     */
    @Override
    public Set<ZSetOperations.TypedTuple<Object>> zRangeByScore(String key, int from, long to) {
        return Set.of();
    }

    /**
     * 为测试替身未实现的有序集合删除操作返回零。
     *
     * @param key   缓存键
     * @param value 测试值
     * @return 零，测试替身不执行集合删除
     */
    @Override
    public Long zRemove(String key, String... value) {
        return 0L;
    }
}
