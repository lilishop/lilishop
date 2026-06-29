package cn.lili.modules.live.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.live.entity.dos.LiveBlock;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
public interface LiveBlockService extends IService<LiveBlock> {
    /**
     * 根据直播间ID查询拉黑用户列表
     *
     * @return 拉黑用户列表
     */
    List<LiveBlock> liveBlockList();
    /**
     * 根据直播间ID分页查询拉黑用户列表
     *
     * @param page 分页信息
     * @return 拉黑用户列表
     */
    IPage<LiveBlock> liveBlockPage(PageVO page);
    /**
     * 拉黑用户
     *
     * @param userId    用户ID
     * @param liveRoomId 直播间ID
     * @param reason    拉黑原因
     */
    void blockUser(String userId, String liveRoomId, String reason);

     /**
      * 取消拉黑用户
      *
      * @param userId    用户ID
      * @param liveRoomId 直播间ID
      */
    void unblockUser(String userId, String liveRoomId);
}
