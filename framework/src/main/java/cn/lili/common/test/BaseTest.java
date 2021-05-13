package cn.lili.common.test;

import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.annotation.Rollback;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestContext;
import org.springframework.test.context.TestExecutionListener;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;

/**
 * BaseTest
 *
 * @author Chopper
 * @version v1.0
 * @since
 * 2020-06-13 12:17
 */
@RunWith(SpringRunner.class)
@SpringBootTest
@Transactional(rollbackFor = Exception.class)
@Rollback()
@ContextConfiguration
@Configuration
@ComponentScan("cn.lili")
public class BaseTest implements TestExecutionListener {
    @Override
    public void beforeTestClass(TestContext testContext) throws Exception {
        //设置环境变量 解决es冲突
        System.setProperty("es.set.netty.runtime.available.processors", "false");
    }

}
