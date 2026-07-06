package cn.lili.modules.live.service;

import cn.lili.modules.live.entity.dos.LiveMessage;
import cn.lili.modules.live.entity.dos.LiveUser;

/**
 * 直播操作服务层
 * @author chc
 * @since 2022/6/2114:46
 */
public interface LiveOperateService {

    /**
     * 检查直播用户是否存在
     *
     * @param liveRoomId 直播房间ID
     * @return 直播用户
     */
    LiveUser checkLiveUser(String liveRoomId);

    /**
     * 发送直播房间消息
     *
     * @param liveMessage 直播房间消息
     */
    void sendMessage(LiveMessage liveMessage);

    /**
     * 审核直播房间消息
     *
     * @param liveMessage 直播房间消息
     */
    void authMessage(LiveMessage liveMessage);

    /**
     * 获取观看数量
     * @param liveRoomId
     * @return
     */
    Long getCacheView(String liveRoomId);

}
