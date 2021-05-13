package cn.lili.modules.message.service;

import com.baomidou.mybatisplus.extension.service.IService;
import cn.lili.modules.message.entity.dos.WechatMPMessage;

/**
 * 微信小程序消息订阅 业务层
 * @author Chopper
 */
public interface WechatMPMessageService extends IService<WechatMPMessage> {

    /**
     * 初始化微信消息订阅模版
     */
    void init();
}