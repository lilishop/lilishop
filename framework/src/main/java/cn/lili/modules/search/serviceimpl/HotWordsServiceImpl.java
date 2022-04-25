package cn.lili.modules.search.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.modules.search.entity.dos.HotWordsHistory;
import cn.lili.modules.search.entity.dto.HotWordsDTO;
import cn.lili.modules.search.mapper.HotWordsHistoryMapper;
import cn.lili.modules.search.service.HotWordsService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.ZSetOperations;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * HotWordsServiceImpl
 *
 * @author Chopper
 * @version v1.0
 * 2022-04-14 09:35
 */
@Slf4j
@Service
public class HotWordsServiceImpl implements HotWordsService {

    /**
     * 缓存
     */
    @Autowired
    private Cache<Object> cache;

    @Override
    public List<String> getHotWords(Integer count) {
        if (count == null) {
            count = 0;
        }
        List<String> hotWords = new ArrayList<>();
        // redis 排序中，下标从0开始，所以这里需要 -1 处理
        count = count - 1;
        Set<ZSetOperations.TypedTuple<Object>> set = cache.reverseRangeWithScores(CachePrefix.HOT_WORD.getPrefix(), count);
        if (set == null || set.isEmpty()) {
            return new ArrayList<>();
        }
        for (ZSetOperations.TypedTuple<Object> defaultTypedTuple : set) {
            hotWords.add(Objects.requireNonNull(defaultTypedTuple.getValue()).toString());
        }
        return hotWords;
    }

    @Override
    public List<HotWordsHistory> getHotWordsVO(Integer count) {
        if (count == null) {
            count = 50;
        }
        List<HotWordsHistory> hotWords = new ArrayList<>();
        // redis 排序中，下标从0开始，所以这里需要 -1 处理
        count = count - 1;
        Set<ZSetOperations.TypedTuple<Object>> set = cache.reverseRangeWithScores(CachePrefix.HOT_WORD.getPrefix(), count);
        if (set == null || set.isEmpty()) {
            return new ArrayList<>();
        }
        for (ZSetOperations.TypedTuple<Object> defaultTypedTuple : set) {
            try {
                hotWords.add(new HotWordsHistory(defaultTypedTuple.getValue().toString(),
                        defaultTypedTuple.getScore().intValue()));
            } catch (Exception e) {
                log.error("读取热词错误", e);
            }

        }


        Collections.sort(hotWords);
        return hotWords;
    }

    @Override
    public void setHotWords(HotWordsDTO hotWords) {
        cache.incrementScore(CachePrefix.HOT_WORD.getPrefix(), hotWords.getKeywords(), hotWords.getPoint());
    }

    /**
     * 删除热门关键词
     *
     * @param keywords 热词
     */
    @Override
    public void deleteHotWords(String keywords) {
        cache.zRemove(CachePrefix.HOT_WORD.getPrefix(), keywords);
    }

}
