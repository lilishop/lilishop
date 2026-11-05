package cn.lili.modules.member.service;

import cn.lili.common.exception.ServiceException;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.entity.dos.MemberBenefitDelivery;
import cn.lili.modules.member.entity.dos.PaidMemberOrder;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;
import java.util.function.BiConsumer;

/**
 * 客户权益逐项发放及账本管理接口，按来源和周期防重，单项发放失败独立记录并支持重试。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
public interface MemberBenefitDeliveryService extends IService<MemberBenefitDelivery> {

    /**
     * 初始化等级来源并尝试发放当前应享的自动权益，调用方须先确认客户仍享有该等级权益。
     * 初始化记录与一次性权益预占在同一事务创建；旧初始化记录缺少一次性明细时不重新赠送。
     * 周期按上海时区及初始化日期计算，仅生成当前应发周期；成功记录或尝试次数达到10的记录跳过。
     *
     * @param memberId 领取客户ID
     * @param gradeId 权益来源等级ID
     * @param benefits 该等级当前可发放的权益，非赠送积分及券礼包类型跳过
     * @param grant 实际发放回调，在单项独立事务内执行，失败应抛出异常
     */
    void deliverGrade(String memberId, String gradeId, List<MemberBenefit> benefits,
                             BiConsumer<Member, MemberBenefit> grant);

    /**
     * 按付费会员订单来源生成并发放当前周期权益，周期锚点为订单有效期开始日期（上海时区）。
     * 调用方提供已生效订单及可用权益，实际执行前再次校验付费资格；不同续费订单分别去重。
     * 已成功或尝试次数达到10的记录不自动重发，预占已存在记录时保留原配置快照。
     *
     * @param order 来源会员订单，需包含客户、主键及有效期开始时间
     * @param benefits 按订单配置关联并过滤当前开启状态的权益
     * @param grant 在单项独立事务内执行的实际发放回调
     */
    void deliverPaid(PaidMemberOrder order, List<MemberBenefit> benefits, BiConsumer<Member, MemberBenefit> grant);

    /**
     * 重试指定发放明细，沿用首次预占的权益配置快照。
     * 调用方负责重试入口的状态、领取资格及自动重试次数限制，本方法不额外限制尝试次数。
     *
     * @param record 待重试明细，仅使用主键读取并锁定最新记录
     * @param grant 在单项独立事务内执行的实际发放回调
     * @throws ServiceException 本次执行失败，原因已尝试写入发放明细
     */
    void retry(MemberBenefitDelivery record, BiConsumer<Member, MemberBenefit> grant);
}
