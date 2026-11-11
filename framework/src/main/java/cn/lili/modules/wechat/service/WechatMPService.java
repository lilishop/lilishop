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

    /**
     * 平台管理员按当前真实履约快照单笔补报；不受自动开关限制，但复用全部业务校验。
     * @param orderSn 触发补报的商城订单号，同笔支付下的子订单统一归集
     * @return SUCCESS/SKIPPED/PENDING/FAILED，详细原因写入同步日志
     */
    String retryShippingInfo(String orderSn);

    /**
     * 订单确认收货
     * @param orderSn
     */
    void notifyConfirmReceive(String orderSn);
}
