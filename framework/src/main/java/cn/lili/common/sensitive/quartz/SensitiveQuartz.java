package cn.lili.common.sensitive.quartz;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.sensitive.SensitiveWordsFilter;
import lombok.extern.slf4j.Slf4j;
import org.quartz.JobExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.quartz.QuartzJobBean;

import java.util.List;

/**
 * 间隔更新敏感词
 *
 * @author Chopper
 * @version v1.0
 * 2021-11-23 16:31
 */
@Slf4j
public class SensitiveQuartz extends QuartzJobBean {

    @Autowired
    private Cache<List<String>> cache;

    /**
     * 定时更新敏感词信息
     *
     * @param jobExecutionContext
     */
    @Override
    protected void executeInternal(JobExecutionContext jobExecutionContext) {
        log.info("敏感词定时更新");
        List<String> sensitives = cache.get(CachePrefix.SENSITIVE.getPrefix());
        if (sensitives == null || sensitives.isEmpty()) {
            return;
        }
        SensitiveWordsFilter.init(sensitives);
    }
}