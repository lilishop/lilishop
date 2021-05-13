package cn.lili.common.cache;

import cn.lili.common.utils.SpringContextUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.ibatis.cache.Cache;
import org.springframework.data.redis.connection.RedisServerCommands;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.util.CollectionUtils;

import java.util.Set;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * MybatisRedisCache
 *
 * @author Chopper
 * @version v1.0
 * @since
 * 2020-04-01 2:59 下午
 * 不赞成使用此方式注解，统一使用Cacheable 更为合适
 *
 * 使用方法 @CacheNamespace(implementation= MybatisRedisCache.class,eviction=MybatisRedisCache.class)
 */
@Deprecated
@Slf4j
public class MybatisRedisCache implements Cache {

    private final ReadWriteLock readWriteLock = new ReentrantReadWriteLock(true);


    private RedisTemplate<Object, Object> getRedisTemplate() {
        return (RedisTemplate<Object, Object>) SpringContextUtil.getBean("redisTemplate");
    }

    private final String id;

    public MybatisRedisCache(final String id) {
        if (id == null) {
            throw new IllegalArgumentException("Cache instances require an ID");
        }
        this.id = id;
    }

    @Override
    public String getId() {
        return this.id;
    }

    @Override
    public void putObject(Object key, Object value) {
        try {
            if (value != null) {
                log.info("写入缓存：" + key.toString()+"----"+value.toString());
                getRedisTemplate().opsForValue().set(key.toString(), value);
            }
        } catch (Exception e) {
            log.error("写入mybatis缓存异常 ", e);
        }
    }

    @Override
    public Object getObject(Object key) {
        try {
            if (key != null) {
                log.info("获取缓存：" + key);
                return getRedisTemplate().opsForValue().get(key.toString());
            }
        } catch (Exception e) {
            log.error("mybatis缓存获取异常 ", e);
        }
        return null;
    }

    @Override
    public Object removeObject(Object key) {
        if (key != null) {
            getRedisTemplate().delete(key.toString());
        }
        return null;
    }

    @Override
    public void clear() {
        Set<Object> keys = getRedisTemplate().keys("*:" + this.id + "*");
        if (!CollectionUtils.isEmpty(keys)) {
            getRedisTemplate().delete(keys);
        }
    }

    @Override
    public int getSize() {
        Long size = getRedisTemplate().execute(RedisServerCommands::dbSize);
        return size.intValue();
    }

    @Override
    public ReadWriteLock getReadWriteLock() {
        return this.readWriteLock;
    }
}
