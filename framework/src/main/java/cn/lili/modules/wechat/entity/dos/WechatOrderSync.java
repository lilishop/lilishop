package cn.lili.modules.wechat.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.EqualsAndHashCode;
import java.util.Date;

/**
 * 按微信支付流水归集的发货同步状态，不复制商城订单业务数据。
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("li_wechat_order_sync")
public class WechatOrderSync extends BaseIdEntity {
    /** 收货通知状态：null 尚未成功通知、SUCCESS 微信已受理；不表示消费者主动确认收货。 */
    private String receiveStatus;

    /** 最近一次收货通知成功时间，Asia/Shanghai。 */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date receiveTime;
    /** 对应 li_order.receivable_no；同笔合并支付共用一条记录，缺流水异常使用 ORDER:商城订单号占位。 */
    private String transactionId;

    /** 执行上报时的小程序 AppID 快照，不保存密钥。 */
    private String appId;

    /** 执行上报时的微信支付商户号快照。 */
    private String mchId;

    /** 发货上报状态：PENDING 待上报、PROCESSING 处理中、SUCCESS 成功、FAILED 失败；无记录由查询端显示 UNCHECKED 待核查。 */
    private String status;

    /** 发货请求内容 SHA-256 摘要，不包含上传时间，用于重复事件去重。 */
    private String requestHash;

    /** 最近已获微信成功响应的请求摘要，与最近一次失败尝试摘要分开，防止暂停或失败后重复提交成功版本。 */
    private String successHash;

    /** 该版本请求的微信上传时间，ISO-8601 带时区；相同请求重试复用该值。 */
    private String uploadTime;

    /** 最近发货上报错误码；校验错误为业务码，微信拒绝为接口错误码。 */
    private String errorCode;

    /** 最近发货上报的脱敏错误说明，最多 500 字符。 */
    private String errorMessage;

    /** 最近一次发货上报尝试时间，Asia/Shanghai。 */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date lastAttemptTime;

    /** 最近一次发货上报成功时间，Asia/Shanghai；不表示微信确认收货或结算成功。 */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date successTime;

    /** 记录创建时间，Asia/Shanghai。 */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;

    /** 最近状态更新时间，Asia/Shanghai。 */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date updateTime;

}
