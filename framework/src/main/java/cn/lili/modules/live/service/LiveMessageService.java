package cn.lili.modules.live.service;

import cn.lili.modules.live.entity.dos.LiveMessage;
import cn.lili.modules.live.entity.dto.LiveMessageSearchDTO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
public interface LiveMessageService extends IService<LiveMessage> {

    /**
     * 查询直播消息分页
     * @param liveMessageSearchDTO 查询参数
     * @return 直播消息分页
     */
    IPage<LiveMessage> queryMessagePage(LiveMessageSearchDTO liveMessageSearchDTO);

     /**
      * 根据用户ID删除直播消息
      * @param userId 用户ID
      * @return 是否删除成功
      */
     boolean removeByUserId(String liveId, String userId);
}
