package cn.lili.modules.wechat.service;

import cn.lili.modules.wechat.entity.dos.WechatMPMessage;
import com.baomidou.mybatisplus.extension.service.IService;

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