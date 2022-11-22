package cn.lili.im.service;

import cn.lili.im.entity.ImTalk;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 聊天 业务层
 *
 * @author Chopper
 */
public interface ImTalkService extends IService<ImTalk> {

    /**
     * 获取与某人的聊天框
     *
     * @param userId1
     * @param userId2
     * @return
     */
    ImTalk getTalkByUser(String userId1, String userId2);

    /**
     * 置顶消息
     *
     * @param id
     * @param top
     */
    void top(String id, Boolean top);

    /**
     * 禁用（前端不做展示）聊天
     *
     * @param id
     */
    void disable(String id);
}