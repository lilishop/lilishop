package cn.lili.modules.wechat.service;
/**
 * 微信小程序 业务层
 * @author Chopper
 */
public interface WechatMPService {


    /**
     * 微信小程序-上传发货信息
     * @param orderSn
     */
    void uploadShippingInfo(String orderSn);
}
