package cn.lili.modules.live.service;

import cn.lili.cache.limit.annotation.RedisCacheable;
import cn.lili.modules.live.entity.vos.LivePollingDataVO;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
public interface LivePollingDataCacheService {

    /**
     * 获取直播轮询公共数据（缓存 10 秒）。
     * 注意：该方法不包含用户维度领取记录，防止缓存串用户数据。
     * @param liveId
     * @return
     */
    @RedisCacheable(key = "'LivePollingGlobal_' + #liveId", ttl = 10)
    LivePollingDataVO getGlobalPollingData(String liveId);
}
