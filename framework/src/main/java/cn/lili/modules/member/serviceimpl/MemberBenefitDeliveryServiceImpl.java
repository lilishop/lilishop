package cn.lili.modules.member.serviceimpl;

import cn.hutool.crypto.digest.DigestUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.member.entity.dos.*;
import cn.lili.modules.member.mapper.MemberBenefitDeliveryMapper;
import cn.lili.modules.member.mapper.MemberBenefitMapper;
import cn.lili.modules.member.mapper.MemberGradeBenefitGrantMapper;
import cn.lili.modules.member.mapper.PaidMemberOrderMapper;
import cn.lili.modules.member.service.MemberBenefitDeliveryService;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.member.util.MemberBenefitRules;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.support.TransactionTemplate;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.List;
import java.util.function.BiConsumer;

/**
 * 客户权益逐项发放服务，统一处理普通等级及付费会员的赠送积分、一次性券礼包和周期券礼包。
 * 按客户、来源类型、来源记录、权益及周期生成防重账本；每项发放独立提交，失败另开事务记录原因。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Service("memberBenefitDeliveryService")
@Slf4j
public class MemberBenefitDeliveryServiceImpl extends ServiceImpl<MemberBenefitDeliveryMapper, MemberBenefitDelivery> implements MemberBenefitDeliveryService {

    @Autowired
    private MemberGradeBenefitGrantMapper gradeGrantMapper;
    @Autowired
    private MemberService memberService;
    @Autowired
    private PlatformTransactionManager transactionManager;
    @Autowired
    private PaidMemberOrderMapper paidOrders;
    @Autowired
    private MemberBenefitMapper benefits;

    /**
     * 创建 REQUIRES_NEW 事务模板，挂起调用方事务并独立提交或回滚。
     *
     * @return 用于预占账本、发放权益或记录失败的事务模板
     */
    private TransactionTemplate transaction() {
        TransactionTemplate tx = new TransactionTemplate(transactionManager);
        tx.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW);
        return tx;
    }

    /**
     * 初始化等级来源并尝试发放当前应享的自动权益，调用方须先确认客户仍享有该等级权益。
     * 初始化记录与一次性权益预占在同一事务创建；旧初始化记录缺少一次性明细时不重新赠送。
     * 周期按上海时区及初始化日期计算，仅生成当前应发周期；成功记录或尝试次数达到10的记录跳过。
     *
     * @param memberId 领取客户ID
     * @param gradeId  权益来源等级ID
     * @param benefits 该等级当前可发放的权益，非赠送积分及券礼包类型跳过
     * @param grant    实际发放回调，在单项独立事务内执行，失败应抛出异常
     */
    @Override
    public void deliverGrade(String memberId, String gradeId, List<MemberBenefit> benefits,
                             BiConsumer<Member, MemberBenefit> grant) {
        // 兼容旧记录：旧等级已发放的一次性权益不会迁移后再次赠送。
        boolean first = Boolean.TRUE.equals(transaction().execute(tx -> {
            MemberGradeBenefitGrant prior = gradeGrantMapper.selectOne(new LambdaQueryWrapper<MemberGradeBenefitGrant>()
                    .eq(MemberGradeBenefitGrant::getMemberId, memberId).eq(MemberGradeBenefitGrant::getGradeId, gradeId));
            if (prior != null) return false;
            MemberGradeBenefitGrant claim = new MemberGradeBenefitGrant();
            claim.setMemberId(memberId);
            claim.setGradeId(gradeId);
            try {
                gradeGrantMapper.insert(claim);
                for (MemberBenefit benefit : benefits) {
                    if ("GIFT_POINT".equals(benefit.getBenefitType()) || ("COUPON_PACKAGE".equals(benefit.getBenefitType())
                            && "ONCE".equals(MemberBenefitRules.config(benefit.getBenefitConfig()).getStr("grantRule", "ONCE")))) {
                        baseMapper.reserve(snapshot(memberId, gradeId, benefit, "ONCE"));
                    }
                }
                return true;
            } catch (org.springframework.dao.DuplicateKeyException e) {
                return false;
            }
        }));
        MemberGradeBenefitGrant reached = gradeGrantMapper.selectOne(new LambdaQueryWrapper<MemberGradeBenefitGrant>()
                .eq(MemberGradeBenefitGrant::getMemberId, memberId).eq(MemberGradeBenefitGrant::getGradeId, gradeId));
        if (reached == null) return;
        ZoneId zone = ZoneId.of("Asia/Shanghai");
        LocalDate since = reached.getCreateTime().toInstant().atZone(zone).toLocalDate();
        for (MemberBenefit benefit : benefits) {
            if (!"GIFT_POINT".equals(benefit.getBenefitType()) && !"COUPON_PACKAGE".equals(benefit.getBenefitType()))
                continue;
            String cycle = "GIFT_POINT".equals(benefit.getBenefitType()) ? "ONCE"
                    : MemberBenefitRules.cycle(MemberBenefitRules.config(benefit.getBenefitConfig()), since, LocalDate.now(zone));
            if (cycle == null) continue;
            MemberBenefitDelivery record = snapshot(memberId, gradeId, benefit, cycle);
            MemberBenefitDelivery previous = getById(record.getId());
            if ("ONCE".equals(cycle) && !first && previous == null) continue;
            if (previous != null && ("SUCCESS".equals(previous.getStatus()) || previous.getAttempts() >= 10)) continue;
            transaction().executeWithoutResult(tx -> baseMapper.reserve(record));
            execute(record.getId(), grant);
        }
    }

    /**
     * 构造等级来源的发放快照，以客户、来源、权益和周期的摘要作为确定性主键。
     * 付费来源复用快照字段后须改写来源类型及主键，避免与等级来源混淆。
     *
     * @param memberId 领取客户ID
     * @param gradeId  来源引用，等级发放传等级ID，付费发放复用时传会员订单主键
     * @param benefit  待保存名称、类型和配置快照的权益
     * @param cycle    发放周期键，一次性权益为 ONCE，周期券由周期规则生成
     * @return 尚未持久化的发放明细
     */
    private MemberBenefitDelivery snapshot(String memberId, String gradeId, MemberBenefit benefit, String cycle) {
        MemberBenefitDelivery record = new MemberBenefitDelivery();
        record.setId(DigestUtil.md5Hex(memberId + "|GRADE|" + gradeId + "|" + benefit.getId() + "|" + cycle));
        record.setMemberId(memberId);
        record.setSourceType("GRADE");
        record.setSourceRef(gradeId);
        record.setBenefitId(benefit.getId());
        record.setBenefitName(benefit.getBenefitName());
        record.setBenefitType(benefit.getBenefitType());
        record.setBenefitConfig(benefit.getBenefitConfig());
        record.setCycleKey(cycle);
        return record;
    }

    /**
     * 按付费会员订单来源生成并发放当前周期权益，周期锚点为订单有效期开始日期（上海时区）。
     * 调用方提供已生效订单及可用权益，实际执行前再次校验付费资格；不同续费订单分别去重。
     * 已成功或尝试次数达到10的记录不自动重发，预占已存在记录时保留原配置快照。
     *
     * @param order    来源会员订单，需包含客户、主键及有效期开始时间
     * @param benefits 按订单配置关联并过滤当前开启状态的权益
     * @param grant    在单项独立事务内执行的实际发放回调
     */
    @Override
    public void deliverPaid(PaidMemberOrder order, List<MemberBenefit> benefits, BiConsumer<Member, MemberBenefit> grant) {
        ZoneId zone = ZoneId.of("Asia/Shanghai");
        for (MemberBenefit benefit : benefits) {
            if (!"GIFT_POINT".equals(benefit.getBenefitType()) && !"COUPON_PACKAGE".equals(benefit.getBenefitType()))
                continue;
            String cycle = "GIFT_POINT".equals(benefit.getBenefitType()) ? "ONCE" : MemberBenefitRules.cycle(
                    MemberBenefitRules.config(benefit.getBenefitConfig()), order.getStartTime().toInstant().atZone(zone).toLocalDate(), LocalDate.now(zone));
            if (cycle == null) continue;
            MemberBenefitDelivery record = snapshot(order.getMemberId(), order.getId(), benefit, cycle);
            record.setSourceType("PAID_MEMBER");
            record.setId(DigestUtil.md5Hex(order.getMemberId() + "|PAID_MEMBER|" + order.getId() + "|" + benefit.getId() + "|" + cycle));
            MemberBenefitDelivery previous = getById(record.getId());
            if (previous != null && ("SUCCESS".equals(previous.getStatus()) || previous.getAttempts() >= 10)) continue;
            transaction().executeWithoutResult(tx -> baseMapper.reserve(record));
            execute(record.getId(), grant);
        }
    }

    /**
     * 重试指定发放明细，沿用首次预占的权益配置快照。
     * 调用方负责重试入口的状态、领取资格及自动重试次数限制，本方法不额外限制尝试次数。
     *
     * @param record 待重试明细，仅使用主键读取并锁定最新记录
     * @param grant  在单项独立事务内执行的实际发放回调
     * @throws ServiceException 本次执行失败，原因已尝试写入发放明细
     */
    @Override
    public void retry(MemberBenefitDelivery record, BiConsumer<Member, MemberBenefit> grant) {
        if (!execute(record.getId(), grant))
            throw new ServiceException(ResultCode.PARAMS_ERROR, "补发失败，请查看发放记录中的原因");
    }

    /**
     * 锁定发放明细及客户，按权益快照执行回调，并在同一事务中保存成功状态和尝试次数。
     * 付费来源再次核验订单归属、已支付状态、左闭右开有效区间、快照关联及权益当前开启状态。
     * 回调异常时回滚发放，再独立记录失败及截取到500字符的原因；成功记录直接跳过。
     *
     * @param id    发放明细主键
     * @param grant 实际发放回调，异常将触发单项事务回滚
     * @return true 表示事务正常结束（含记录不存在或已成功而跳过），false 表示执行异常且已尝试记录失败
     */
    private boolean execute(String id, BiConsumer<Member, MemberBenefit> grant) {
        try {
            transaction().executeWithoutResult(tx -> {
                MemberBenefitDelivery record = baseMapper.lock(id);
                if (record == null || "SUCCESS".equals(record.getStatus())) return;
                Member member = memberService.getOne(new LambdaQueryWrapper<Member>().eq(Member::getId, record.getMemberId()).last("FOR UPDATE"));
                if (member == null) throw new ServiceException(ResultCode.USER_NOT_EXIST);
                if ("PAID_MEMBER".equals(record.getSourceType())) {
                    var order = paidOrders.selectById(record.getSourceRef());
                    var currentBenefit = benefits.selectById(record.getBenefitId());
                    java.util.Date now = new java.util.Date();
                    if (order == null || !order.getMemberId().equals(record.getMemberId()) || !"PAID".equals(order.getStatus()) || order.getStartTime().after(now) || !order.getEndTime().after(now)
                            || currentBenefit == null || !"OPEN".equals(currentBenefit.getBenefitState())
                            || !cn.hutool.json.JSONUtil.toBean(order.getConfigSnapshot(), cn.lili.modules.system.entity.dto.PaidMemberSetting.class).getBenefitIds().contains(record.getBenefitId()))
                        throw new ServiceException(ResultCode.PARAMS_ERROR, "付费会员权益已失效");
                }
                MemberBenefit benefit = new MemberBenefit();
                benefit.setId(record.getBenefitId());
                benefit.setBenefitName(record.getBenefitName());
                benefit.setBenefitType(record.getBenefitType());
                benefit.setBenefitConfig(record.getBenefitConfig());
                grant.accept(member, benefit);
                record.setStatus("SUCCESS");
                record.setAttempts(record.getAttempts() + 1);
                record.setErrorMessage("");
                baseMapper.updateById(record);
            });
            return true;
        } catch (Exception e) {
            String reason = e.getMessage() == null ? "发放失败" : e.getMessage();
            String message = reason.substring(0, Math.min(500, reason.length()));
            transaction().executeWithoutResult(tx -> baseMapper.failed(id, message));
            log.warn("权益发放失败 recordId={}: {}", id, message);
            return false;
        }
    }
}
