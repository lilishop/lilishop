package cn.lili.modules.member.entity.dos;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;
import java.math.BigDecimal;
import java.util.Date;
/**
 * 付费会员开通、续费的会员费订单，对应 li_paid_member_order 表。
 *
 * <p>每次购买创建一笔独立订单，保存当时的金额、有效月数和会员配置快照。
 * 支付成功后才生成权益有效区间；有效期内续费接续最后一笔已支付订单的到期时间，
 * 首次购买或到期后重购从当前支付处理时间开始。</p>
 *
 * <p>订单支付状态与会员资格状态分别判断：PAID 订单可能尚未生效或已经到期，
 * 当前资格以 startTime 小于等于当前时间且 endTime 大于当前时间为准。
 * 超时不新增支付状态，而是通过 payDeadline 判断是否允许再次发起支付。</p>
 *
 * <p>继承的 id 是订单内部主键，用于权益来源和商品补贴归属；sn 是收银台使用的业务订单号。
 * 本表不承载普通商品订单的收货地址、物流或发货流程。</p>
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("li_paid_member_order")
public class PaidMemberOrder extends BaseEntity {
    /** 会员费业务订单号，格式为 PM 加订单内部 ID，全局唯一，用于收银台、支付通知及记录查询。 */
    private String sn;

    /** 购买付费会员的客户 ID，对应 li_member.id；所有买家订单操作需校验归属。 */
    private String memberId;

    /** 创建订单时的客户昵称快照，用于管理端展示和搜索，不随客户后续改名更新。 */
    private String memberName;

    /** 客户端购买请求的幂等编号；与 memberId 构成唯一键，同一请求重试返回原订单。 */
    private String requestId;

    /** 本次开通或续费的应付会员费，单位元、精确到分；服务端支付通知必须核对一致。 */
    private BigDecimal amount;

    /** 本次购买的有效期月数，按上海时区的自然月计算；月末不足对应日期时取目标月末。 */
    private Integer months;

    /**
     * 支付状态：UNPAID 未支付、PAID 已支付、CANCELLED 已取消。
     * 正常流转为 UNPAID 到 PAID 或 CANCELLED；已取消/超时后收到经校验的真实到账通知，
     * 仍允许认领为 PAID，避免已扣款未开通；重复成功通知不得再次延长有效期。
     */
    private String status;

    /** 创建订单时的 PaidMemberSetting JSON 快照，保留成交价格、期限、权益关联和商品折扣等规则。 */
    private String configSnapshot;

    /** 创建订单时配置 JSON 的 SHA-256 摘要，用于识别成交配置版本及关联商品优惠快照。 */
    private String configVersion;

    /** 允许发起支付的截止时间，创建订单后 30 分钟；过期不再发起支付，但仍处理真实到账通知。 */
    private Date payDeadline;

    /** 服务端首次确认有效支付通知的处理时间；未支付时为空，不直接作为续费权益起始时间。 */
    private Date payTime;

    /** 本笔订单权益的生效时间，包含该时刻；未支付时为空，有效期内续费时可能晚于支付时间。 */
    private Date startTime;

    /** 本笔订单权益的到期时间，不包含该时刻；由 startTime 加 months 个自然月计算，未支付时为空。 */
    private Date endTime;

    /** 实际支付渠道：WECHAT 微信、ALIPAY 支付宝、WALLET 余额；支付成功时记录。 */
    private String paymentMethod;

    /** 支付成功通知提供的收款流水号，用于交易对账和追溯；未支付时为空。 */
    private String receivableNo;

}
