package cn.lili.test.script;

import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.ClassPathResource;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.scripting.support.ResourceScriptSource;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.List;

/**
 * redis 事务测试
 *
 * @author Chopper
 * @version v1.0
 * @since
 * 2020-02-22 20:26
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest
@Rollback()
@ContextConfiguration
@Configuration
@ComponentScan("cn.lili")
public class ScriptTest {
    @Resource
    private DefaultRedisScript<Boolean> redisScript;
    @Resource
    private StringRedisTemplate stringRedisTemplate;

    @Test
    public void lua() {
        stringRedisTemplate.opsForValue().set("key_1", "100");
        stringRedisTemplate.opsForValue().set("key_2", "95");
        stringRedisTemplate.opsForValue().set("key_3", "90");
        stringRedisTemplate.opsForValue().set("key_4", "85");
        List<String> keys = new ArrayList<>();
        keys.add("key_1");
        keys.add("key_2");
        keys.add("key_3");
        keys.add("key_4");
        List<String> value = new ArrayList<>();
        value.add("-1");
        value.add("-1");
        value.add("-1");
        value.add("-1");
        //启用十个线程
        for (int i = 0; i <= 10; i++) {
            //每个线程循环十次
            Thread thread = new Thread(() -> {
                for (int i1 = 0; i1 <= 10; i1++) {
                    Boolean execute = stringRedisTemplate.execute(redisScript, keys, value.toArray());
                    System.out.println(Thread.currentThread().getName() + "|" + i1 + "|" + execute);
                }
            });
            thread.start();
        }
    }

}

@Configuration
class LuaConfiguration {
    @Bean
    public DefaultRedisScript<Boolean> redisScript() {
        DefaultRedisScript<Boolean> redisScript = new DefaultRedisScript<>();
        redisScript.setScriptSource(new ResourceScriptSource(new ClassPathResource("script/quantity.lua")));
        redisScript.setResultType(Boolean.class);
        return redisScript;
    }
}
