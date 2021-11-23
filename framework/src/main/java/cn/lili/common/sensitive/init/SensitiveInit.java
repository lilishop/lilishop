package cn.lili.cache.impl;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.sensitive.SensitiveWordsFilter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 初始化敏感词
 *
 * @author Chopper
 * @version v1.0
 * 2021-11-23 12:08
 */
@Component
public class SensitiveInit implements ApplicationRunner {

    @Autowired
    private Cache<List<String>> cache;

    /**
     * 程序启动时，获取最新的需要过滤的敏感词
     *
     * @param args
     */
    @Override
    public void run(ApplicationArguments args) {
        List<String> sensitives = cache.get(CachePrefix.SENSITIVE.getPrefix());
        if (sensitives == null || sensitives.isEmpty()) {
            return;
        }
        SensitiveWordsFilter.init(sensitives);
    }

}
