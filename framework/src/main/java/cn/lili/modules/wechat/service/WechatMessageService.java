package cn.lili.modules.wechat.service;

import cn.lili.modules.wechat.entity.dos.WechatMessage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 微信消息 业务层
 * @author Chopper
 */
public interface WechatMessageService extends IService<WechatMessage> {

    /**
     * 初始化微信消息模版
     */
    void init();
}