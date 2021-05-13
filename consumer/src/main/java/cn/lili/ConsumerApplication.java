package cn.lili;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * 消费者
 *
 * @author Chopper
 * @date 2020/11/16 10:03 下午
 */
@SpringBootApplication
public class ConsumerApplication {

    public static void main(String[] args) {
        System.setProperty("es.set.netty.runtime.available.processors", "false");
        SpringApplication.run(ConsumerApplication.class, args);
    }

}