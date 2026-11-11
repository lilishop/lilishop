package cn.lili.modules.wechat.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.EqualsAndHashCode;
import java.util.Date;

/**
 * 微信订单上报审计日志；请求只保留摘要，保护支付及用户凭据。
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("li_wechat_order_sync_log")
public class WechatOrderSyncLog extends BaseIdEntity {
    /** 关联 li_wechat_order_sync.transaction_id，同笔支付下所有子订单可查看。 */
    private String transactionId;

    /** 触发本次操作的商城订单号，对应 li_order.sn。 */
    private String orderSn;

    /** 操作类型：SHIPPING 发货上报、RECEIVE 收货通知。 */
    private String operation;

    /** 触发来源：AUTO 订单事件、MANUAL 管理员单笔补报。 */
    private String triggerType;

    /** 人工操作对应管理员 ID，自动事件为 SYSTEM。 */
    private String operatorId;

    /** 管理员姓名快照，自动事件为系统。 */
    private String operatorName;

    /** 执行结果：PROCESSING 处理中、SUCCESS 成功、FAILED 失败、PENDING 条件未满足、SKIPPED 相同内容已成功。 */
    private String status;

    /** 请求内容摘要，不保存手机号、OpenID、访问令牌或原始请求。 */
    private String requestHash;

    /** 请求摘要，只包含关联商城订单号、包裹数量和配送类型。 */
    private String summary;

    /** 本次操作的业务错误码或微信接口错误码。 */
    private String errorCode;

    /** 本次操作的脱敏错误说明，最多 500 字符。 */
    private String errorMessage;

    /** 本次操作开始时间，Asia/Shanghai。 */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    /** 本次操作结束时间，Asia/Shanghai。 */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date finishTime;

    /** 本次操作耗时，单位毫秒。 */
    private Long durationMs;

}
