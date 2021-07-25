package cn.lili.timetask.handler.impl.hotwords;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.timetask.handler.EveryDayExecute;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * @author paulG
 * @since 2021/3/11
 **/
@Slf4j
@Component
public class HotWordsEveryDayTaskExecute implements EveryDayExecute {

    @Autowired
    private Cache cache;

    /**
     * 执行每日任务
     */
    @Override
    public void execute() {
        //移除昨日的热搜词
        cache.remove(CachePrefix.HOT_WORD.getPrefix());
    }

}
