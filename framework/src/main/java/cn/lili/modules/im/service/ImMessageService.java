package cn.lili.modules.im.service;

import cn.lili.modules.im.entity.dos.ImMessage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * Im消息 业务层
 *
 * @author Chopper
 */
public interface ImMessageService extends IService<ImMessage> {

    /**
     * 阅读消息
     *
     * @param talkId
     * @param accessToken
     */
    void read(String talkId, String accessToken);

    /**
     * 未读消息列表
     *
     * @param accessToken
     */
    List<ImMessage> unReadMessages(String accessToken);

    /**
     * 历史消息
     *
     * @param accessToken
     * @param to
     */
    List<ImMessage> historyMessage(String accessToken, String to);

    /**
     * 是否有新消息
     * @param accessToken
     * @return
     */
    Boolean hasNewMessage(String accessToken);
}