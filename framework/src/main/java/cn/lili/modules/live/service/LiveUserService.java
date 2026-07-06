package cn.lili.modules.live.service;

import cn.lili.modules.live.entity.dos.LiveUser;
import cn.lili.modules.live.entity.dto.LiveUserSearchDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
public interface LiveUserService extends IService<LiveUser> {
    /**
     * 编辑直播用户禁言标识
     *
     * @param liveUserid 直播用户ID
     * @param muteFlag   禁言标识
     */
    void editLiveUserMuteFlag(String liveUserid , Boolean muteFlag);

    /**
     * 更新直播用户
     * @param liveUser 直播用户
     */
    void updateUser(LiveUser liveUser);

    /**
     * 查询直播用户
     * @param liveId
     * @param userId
     * @return
     */
    LiveUser queryLiveUserDetail(String liveId, String userId);
    /**
     * 查询直播用户列表
     * * @param liveUserSearchDTO
     * @return
     */
    IPage<LiveUser> queryLiveUserList(LiveUserSearchDTO liveUserSearchDTO);

    /**
     * 直播用户列表
     * @param liveUserSearchDTO
     * @return
     */
    List<LiveUser> getLiveUserList(LiveUserSearchDTO liveUserSearchDTO);

    /**
     * 查询直播用户数量
     * @param liveRoomId
     * @return
     */
    Long queryLiveUserCount(String liveRoomId);

     /**
     * 直播用户增加金额
     * @param order
     */
    void addAmount(Order order);
    /**
     * 设置直播用户观看时间
     * @param liveId 直播ID
     * @param userId 用户ID
     * @param watchTime 观看时间
     */
    void setUserWatchTime(String liveId, String userId, String watchTime);

    /**
     * 获取或创建直播间用户（并发安全，依赖 live_room_id + user_id 唯一约束）
     *
     * @param liveUser 待创建的直播间用户
     * @return 已存在或新创建的用户
     */
    LiveUser getOrCreate(LiveUser liveUser);

}
