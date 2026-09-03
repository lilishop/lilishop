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

    @Override
    public RebuildStatus get(Object key) {
        return store.get(key);
    }

    @Override
    public String getString(Object key) {
        RebuildStatus value = store.get(key);
        return value == null ? null : value.toString();
    }

    @Override
    public List multiGet(Collection keys) {
        return List.of();
    }

    @Override
    public void multiSet(Map map) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void multiDel(Collection keys) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void put(Object key, RebuildStatus value) {
        store.put(key, value);
    }

    @Override
    public void put(Object key, RebuildStatus value, Long exp) {
        store.put(key, value);
    }

    @Override
    public void put(Object key, RebuildStatus value, Long exp, TimeUnit timeUnit) {
        store.put(key, value);
    }

    @Override
    public Boolean remove(Object key) {
        return store.remove(key) != null;
    }

    @Override
    public void vagueDel(Object key) {
        store.remove(key);
    }

    @Override
    public void clear() {
        store.clear();
    }

    @Override
    public void putHash(Object key, Object hashKey, Object hashValue) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void putAllHash(Object key, Map map) {
        throw new UnsupportedOperationException();
    }

    @Override
    public RebuildStatus getHash(Object key, Object hashKey) {
        return null;
    }

    @Override
    public Map<Object, Object> getHash(Object key) {
        return Map.of();
    }

    @Override
    public boolean hasKey(Object key) {
        return store.containsKey(key);
    }

    @Override
    public List<Object> keys(String pattern) {
        return List.of();
    }

    @Override
    public List<Object> keysBlock(String pattern) {
        return List.of();
    }

    @Override
    public Long cumulative(Object key, Object value) {
        return 0L;
    }

    @Override
    public Long counter(Object key) {
        return 0L;
    }

    @Override
    public List multiCounter(Collection keys) {
        return List.of();
    }

    @Override
    public Long mergeCounter(Object... key) {
        return 0L;
    }

    @Override
    public Long incr(String key, long liveTime) {
        return 0L;
    }

    @Override
    public Long incr(String key) {
        return 0L;
    }

    @Override
    public void incrementScore(String sortedSetName, String keyword) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void incrementScore(String sortedSetName, String keyword, Integer score) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Set<ZSetOperations.TypedTuple<Object>> reverseRangeWithScores(String sortedSetName, Integer start, Integer end) {
        return Set.of();
    }

    @Override
    public Set<ZSetOperations.TypedTuple<Object>> reverseRangeWithScores(String sortedSetName, Integer count) {
        return Set.of();
    }

    @Override
    public boolean zAdd(String key, long score, String value) {
        return false;
    }

    @Override
    public Set<ZSetOperations.TypedTuple<Object>> zRangeByScore(String key, int from, long to) {
        return Set.of();
    }

    @Override
    public Long zRemove(String key, String... value) {
        return 0L;
    }
}
