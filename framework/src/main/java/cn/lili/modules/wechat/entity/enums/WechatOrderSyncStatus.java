package cn.lili.modules.wechat.entity.enums;

/**
 * 微信发货同步状态；成功仅代表发货接口受理，不代表收货或结算。
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
public enum WechatOrderSyncStatus {
    /** 无本地记录的历史订单；需核查，不推断微信侧状态。 */
    UNCHECKED,
    /** 未满足上报条件，可在条件满足后重新触发。 */
    PENDING,
    /** 正在执行或上次执行中断；分布式锁释放后允许人工重试。 */
    PROCESSING,
    /** 当前请求版本已获微信接口成功响应；内容相同时跳过重复请求。 */
    SUCCESS,
    /** 校验或调用失败，纠正原因后可单笔补报。 */
    FAILED
}
