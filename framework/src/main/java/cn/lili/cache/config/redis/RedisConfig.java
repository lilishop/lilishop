package cn.lili.cache.config.redis;

import cn.hutool.core.text.CharSequenceUtil;
import lombok.extern.slf4j.Slf4j;
import org.redisson.Redisson;
import org.redisson.api.RedissonClient;
import org.redisson.config.ClusterServersConfig;
import org.redisson.config.Config;
import org.redisson.config.SentinelServersConfig;
import org.redisson.config.SingleServerConfig;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.data.redis.autoconfigure.DataRedisProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.cache.CacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Role;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheManager;
import org.springframework.data.redis.cache.RedisCacheWriter;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.RedisSerializationContext;
import org.springframework.data.redis.serializer.StringRedisSerializer;

import java.time.Duration;
import java.util.ArrayList;
import java.util.List;

/**
 * 自定义redis配置
 *
 * @author Chopper
 * @version v4.0
 * @since 2021/3/20 09:37
 */

@Slf4j
@Configuration
@Role(BeanDefinition.ROLE_INFRASTRUCTURE)
@ConditionalOnClass(RedisOperations.class)
@EnableConfigurationProperties(DataRedisProperties.class)
public class RedisConfig {

    private static final String REDIS_PREFIX = "redis://";

    @Value("${lili.cache.timeout:7200}")
    private Integer timeout;

    /**
     * 当有多个管理器的时候，必须使用该注解在一个管理器上注释：表示该管理器为默认的管理器
     *
     * @param connectionFactory 链接工厂
     * @return 缓存
     */
    @Bean
    @Primary
    public CacheManager cacheManager(RedisConnectionFactory connectionFactory) {
        //初始化一个RedisCacheWriter
        RedisCacheWriter redisCacheWriter = RedisCacheWriter.nonLockingRedisCacheWriter(connectionFactory);
        //序列化方式2
        FastJsonRedisSerializer<Object> fastJsonRedisSerializer = new FastJsonRedisSerializer<>(Object.class);
        RedisSerializationContext.SerializationPair<Object> pair = RedisSerializationContext.SerializationPair.fromSerializer(fastJsonRedisSerializer);
        RedisCacheConfiguration defaultCacheConfig = RedisCacheConfiguration.defaultCacheConfig().serializeValuesWith(pair);
        //设置过期时间
        defaultCacheConfig = defaultCacheConfig.entryTtl(Duration.ofSeconds(timeout));

        return new RedisCacheManager(redisCacheWriter, defaultCacheConfig);
    }

    @Bean(name = "redisTemplate")
    @ConditionalOnMissingBean(name = "redisTemplate")
    public RedisTemplate<Object, Object> redisTemplate(LettuceConnectionFactory lettuceConnectionFactory) {
        RedisTemplate<Object, Object> template = new RedisTemplate<>();
        //使用fastjson序列化
        FastJsonRedisSerializer<Object> fastJsonRedisSerializer = new FastJsonRedisSerializer<>(Object.class);
        //value值的序列化采用fastJsonRedisSerializer
        template.setValueSerializer(fastJsonRedisSerializer);
        template.setHashValueSerializer(fastJsonRedisSerializer);
        //key的序列化采用StringRedisSerializer
        template.setKeySerializer(new StringRedisSerializer());
        template.setHashKeySerializer(new StringRedisSerializer());
        template.setConnectionFactory(lettuceConnectionFactory);
        return template;
    }

    @Bean(destroyMethod = "shutdown")
    public RedissonClient redisson(DataRedisProperties redisProperties) {
        Config config = new Config();
        if (redisProperties.getSentinel() != null && !redisProperties.getSentinel().getNodes().isEmpty()) {
            // 哨兵模式
            SentinelServersConfig sentinelServersConfig = config.useSentinelServers();
            sentinelServersConfig.setMasterName(redisProperties.getSentinel().getMaster());
            List<String> sentinelAddress = new ArrayList<>();
            for (String node : redisProperties.getCluster().getNodes()) {
                sentinelAddress.add(REDIS_PREFIX + node);
            }
            sentinelServersConfig.setSentinelAddresses(sentinelAddress);
            if (CharSequenceUtil.isNotEmpty(redisProperties.getSentinel().getPassword())) {
                sentinelServersConfig.setSentinelPassword(redisProperties.getSentinel().getPassword());
            }
        } else if (redisProperties.getCluster() != null && !redisProperties.getCluster().getNodes().isEmpty()) {
            // 集群模式
            ClusterServersConfig clusterServersConfig = config.useClusterServers();
            List<String> clusterNodes = new ArrayList<>();
            for (String node : redisProperties.getCluster().getNodes()) {
                clusterNodes.add(REDIS_PREFIX + node);
            }
            clusterServersConfig.setNodeAddresses(clusterNodes);
            if (CharSequenceUtil.isNotEmpty(redisProperties.getPassword())) {
                clusterServersConfig.setPassword(redisProperties.getPassword());
            }
        } else {
            SingleServerConfig singleServerConfig = config.useSingleServer();
            singleServerConfig.setAddress(REDIS_PREFIX + redisProperties.getHost() + ":" + redisProperties.getPort());
            if (CharSequenceUtil.isNotEmpty(redisProperties.getPassword())) {
                singleServerConfig.setPassword(redisProperties.getPassword());
            }
        }

        return Redisson.create(config);
    }

}