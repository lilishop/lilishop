package cn.lili.init;

import cn.lili.modules.system.service.SensitiveWordsService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

/**
 * SensitiveWordsInit
 *
 * @author Chopper
 * @version v1.0
 * 2021-11-29 11:38
 */
@Slf4j
@Component
public class SensitiveWordsInit implements ApplicationRunner {

    @Autowired
    private SensitiveWordsService sensitiveWordsService;

    /**
     * consumer 启动时，实时更新一下过滤词
     *
     * @param args 启动参数
     */
    @Override
    public void run(ApplicationArguments args) {
        sensitiveWordsService.resetCache();
    }

}