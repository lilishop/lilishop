package cn.lili.modules.im.service;

import cn.lili.modules.im.entity.dos.ImTalk;
import cn.lili.modules.im.entity.vo.ImTalkVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

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
     * @return
     */
    ImTalk getTalkByUser(String userId1);

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

    /**
     * 获取用户聊天列表
     * @return
     */
    List<ImTalkVO> getUserTalkList();

    /**
     * 获取商家聊天列表
     * @return
     */
    List<ImTalkVO> getStoreTalkList();
}