package cn.lili;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;

/**
 * 基础API
 *
 * @author Chopper
 * @since 2020/11/17 3:38 下午
 */
@EnableCaching
@SpringBootApplication
public class CommonApiApplication {

    public static void main(String[] args) {
        System.setProperty("rocketmq.client.logUseSlf4j","true");
        SpringApplication.run(CommonApiApplication.class, args);
    }

}
