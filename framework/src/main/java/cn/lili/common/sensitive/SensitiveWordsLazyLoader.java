package cn.lili.common.sensitive;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.utils.SpringContextUtil;
import cn.lili.modules.system.entity.dos.SensitiveWords;
import cn.lili.modules.system.service.SensitiveWordsService;
import lombok.extern.slf4j.Slf4j;

import java.util.List;
import java.util.stream.Collectors;

/**
 * 敏感词懒加载器
 * 过滤时从缓存读取词表，与内存快照比对，变化时才重建过滤结构；
 * 缓存不可用时兜底直查数据库并回填缓存，均失败则本次放行不过滤
 *
 * @author Bulbasaur
 * @date 2026-10-29
 */
@Slf4j
public class SensitiveWordsLazyLoader {

    /**
     * 上次加载的词表快照
     */
    private static volatile List<String> loadedWords;

    /**
     * 词库重建锁
     * SensitiveWordsFilter.init 重建的是静态结构，并发重建会互相干扰，此处串行化
     */
    private static final Object RELOAD_LOCK = new Object();

    /**
     * 确保敏感词库已加载到最新
     *
     * @return true-词库可用（含词表为空），false-加载失败，本次应放行不过滤
     */
    public static boolean ensureLoaded() {
        List<String> words = getWords();
        if (words == null) {
            return false;
        }
        if (words.equals(loadedWords)) {
            return true;
        }
        synchronized (RELOAD_LOCK) {
            if (!words.equals(loadedWords)) {
                SensitiveWordsFilter.init(words);
                loadedWords = words;
                log.info("敏感词库已更新，共加载 {} 个敏感词", words.size());
            }
        }
        return true;
    }

    /**
     * 获取敏感词表
     * 优先读缓存，缓存读不到时兜底直查数据库并回填缓存
     *
     * @return 敏感词表，获取失败返回null
     */
    private static List<String> getWords() {
        Cache cache = null;
        try {
            cache = SpringContextUtil.getBean(Cache.class);
            if (cache != null) {
                List<String> words = (List<String>) cache.get(CachePrefix.SENSITIVE.getPrefix());
                if (words != null) {
                    return words;
                }
            }
        } catch (Exception ignored) {
        }
        //兜底直查数据库
        try {
            SensitiveWordsService sensitiveWordsService = SpringContextUtil.getBean(SensitiveWordsService.class);
            List<String> words = sensitiveWordsService.list().stream()
                    .map(SensitiveWords::getSensitiveWord).collect(Collectors.toList());
            //回填缓存
            if (cache != null) {
                cache.put(CachePrefix.SENSITIVE.getPrefix(), words);
            }
            return words;
        } catch (Exception e) {
            log.warn("敏感词库加载失败，本次放行不过滤", e);
            return null;
        }
    }
}
