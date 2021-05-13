package cn.lili.test.trigger;

import cn.lili.common.cache.Cache;
import cn.lili.common.trigger.interfaces.TimeTriggerExecutor;
import cn.lili.common.utils.DateUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * TestTimeTrigger
 *
 * @author Chopper
 * @version v1.0
 * @since
 * 2019-02-19 下午3:01
 */
@Component
public class TestTimeTrigger implements TimeTriggerExecutor {

    public static String key = "rabbitmq_test_value";
    @Autowired
    private Cache cache;

    /**
     * 执行任务
     *
     * @param object 任务参数
     */
    @Override
    public void execute(Object object) {
        System.out.println(DateUtil.toString(DateUtil.getDateline(), "yyyy-MM-dd HH:mm:ss"));
        System.out.println(key + "===" + object);
        cache.put(key, object);
    }
}
