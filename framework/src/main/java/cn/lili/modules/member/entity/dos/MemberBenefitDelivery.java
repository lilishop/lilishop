package cn.lili.modules.member.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 客户权益发放明细，对应 li_member_benefit_delivery 表。
 *
 * <p>一条记录表示一个客户因某个等级或付费会员订单，在一个发放周期内领取一项权益。
 * 目前用于赠送积分、优惠券礼包等需要实际发放的权益；徽章、客服说明、积分倍率不在此记账。
 * 保留发放时的权益配置快照，用于失败重试、人工补发及结果追溯。</p>
 *
 * <p>同一客户、来源类型、来源 ID、权益 ID、周期标识构成唯一业务记录。
 * 记录 ID 按上述字段生成 MD5，并配合数据库唯一索引及事务行锁防止重复发放。
 * 付费会员续费使用不同的订单 ID 区分新的权益周期。</p>
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("li_member_benefit_delivery")
@Schema(description = "客户权益发放明细")
public class MemberBenefitDelivery extends BaseEntity {
    /** 领取权益的客户 ID，对应 li_member.id。 */
    @Schema(description = "客户ID")
    private String memberId;

    /** 权益来源类型：GRADE 为普通客户等级，PAID_MEMBER 为付费会员订单。 */
    @Schema(description = "来源类型：GRADE/PAID_MEMBER")
    private String sourceType;

    /** 来源记录 ID：GRADE 对应等级 ID，PAID_MEMBER 对应会员费订单 ID（非订单号 sn）。 */
    @Schema(description = "来源ID：等级ID或会员费订单ID")
    private String sourceRef;

    /** 权益定义 ID，对应 li_member_benefit.id。 */
    @Schema(description = "权益ID")
    private String benefitId;

    /** 生成发放记录时的权益名称快照，便于后续查看历史记录。 */
    @Schema(description = "权益名称快照")
    private String benefitName;

    /** 权益类型快照，目前实际发放使用 GIFT_POINT、COUPON_PACKAGE。 */
    @Schema(description = "权益类型快照")
    private String benefitType;

    /** 发放参数 JSON 快照；重试沿用该配置，不按最新积分数量或券礼包内容重新计算。 */
    @Schema(description = "权益发放配置JSON快照")
    private String benefitConfig;

    /** 发放周期标识：ONCE 表示该来源仅发一次，其余周期由 MemberBenefitRules.cycle 生成。 */
    @Schema(description = "周期标识：ONCE或周期规则生成的标识")
    private String cycleKey;

    /** 发放状态：PENDING 待发放、SUCCESS 成功、FAILED 失败；成功后不重复发放。 */
    @Schema(description = "发放状态：PENDING/SUCCESS/FAILED")
    private String status;

    /** 已执行发放的次数，初始为 0；成功或失败均累加，自动重试在累计 10 次后停止。 */
    @Schema(description = "累计发放尝试次数，包含首次执行")
    private Integer attempts;

    /** 最近一次失败原因，最多保留 500 个字符；发放成功后清空。 */
    @Schema(description = "最近一次发放失败原因")
    private String errorMessage;
}
