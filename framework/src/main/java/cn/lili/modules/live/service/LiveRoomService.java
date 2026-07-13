package cn.lili.modules.live.service;

import cn.lili.modules.live.entity.dos.LiveRoom;
import cn.lili.modules.live.entity.dto.LiveRoomSearchDTO;
import cn.lili.modules.live.entity.vos.LiveRoomDetailVO;
import cn.lili.modules.live.entity.vos.LiveRoomVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
public interface LiveRoomService extends IService<LiveRoom> {

    /**
     * 查询直播流分页列表
     *
     * @param liveRoomSearchDTO 直播流查询参数
     * @return 直播流分页列表
     */
    IPage<LiveRoom> queryPage(LiveRoomSearchDTO liveRoomSearchDTO);

    /**
     * 查询直播流列表
     *
     * @param liveRoomSearchDTO 直播流查询参数
     * @return 直播流列表
     */
    List<LiveRoom> queryList(LiveRoomSearchDTO liveRoomSearchDTO);

     /**
      * 创建直播流
      *
      * @param liveRoom 直播流
      * @return 是否创建成功
      */
     boolean createLiveRoom(LiveRoom liveRoom);

     /**
      * 编辑直播流
      *
      * @param liveRoom 直播流
      * @return 是否编辑成功
      */
     boolean editLiveRoom(LiveRoom liveRoom);

     /**
      * 开始直播
      *
      * @param liveRoomId 直播流Id
      */
     void startLiveRoom(String liveRoomId);

     /**
      * 结束直播
      *
      * @param liveRoomId 直播流Id
      */
     void endLiveRoom(String liveRoomId);

     /**
      * 用户获取直播间
      *
      * @param id 直播间Id
      * @return 直播间详情
      */
     LiveRoom getLiveRoomUserDetail(String id);

     /**
      * 编辑直播房间消息权限
      *
      * @param id 直播房间Id
      * @return 是否编辑成功
      */
     boolean editLiveMessageAuth(String id, Boolean authFlag);

    /**
     * 用户获取直播间VO
     *
     * @param id 直播间Id
     * @return 直播间详情VO
     */
     LiveRoomVO getLiveRoomDetailVO(String id);

    /**
     * 获取观看人数
     * @param liveRoomId
     * @return
     */
     Long getViewCount(String liveRoomId);

    /**
     * 获取直播间详情
     * @param liveRoomId
     * @return
     */
    LiveRoomDetailVO queryDetail(String liveRoomId);

}
