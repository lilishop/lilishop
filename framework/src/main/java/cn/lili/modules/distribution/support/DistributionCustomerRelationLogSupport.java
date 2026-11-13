package cn.lili.modules.distribution.support;

import cn.lili.common.security.context.UserContext;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelationLog;
import cn.lili.modules.distribution.entity.enums.DistributionCustomerRelationChangeTypeEnum;
import cn.lili.modules.distribution.entity.enums.DistributionRelationStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionCustomerRelationLogVO;
import cn.lili.modules.distribution.mapper.DistributionCustomerRelationLogMapper;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;

import java.util.Date;
import java.util.List;

/**
 * 分销客户关系变更记录、文案解析与日志展示支持类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
public class DistributionCustomerRelationLogSupport {

    /**
     * 写入客户关系变更日志。
     *
     * @param relationLogMapper 变更日志持久层映射
     * @param relation 当前客户关系
     * @param changeType 变更类型
     * @param beforeDistributionId 变更前分销员ID
     * @param beforeStatus 变更前关系状态
     * @param beforeExpireAt 变更前到期时间
     * @param reason 变更原因
     * @param operatorId 操作人ID（为空时取当前用户或系统）
     */
    public static void writeLog(DistributionCustomerRelationLogMapper relationLogMapper,
                                DistributionCustomerRelation relation,
                                String changeType,
                                String beforeDistributionId,
                                String beforeStatus,
                                Date beforeExpireAt,
                                String reason,
                                String operatorId) {
        DistributionCustomerRelationLog log = new DistributionCustomerRelationLog();
        log.setRelationId(relation.getId());
        log.setMemberId(relation.getMemberId());
        log.setChangeType(changeType);
        log.setBeforeDistributionId(beforeDistributionId);
        log.setAfterDistributionId(relation.getDistributionId());
        log.setBeforeRelationStatus(beforeStatus);
        log.setAfterRelationStatus(relation.getRelationStatus());
        log.setBeforeExpireAt(beforeExpireAt);
        log.setAfterExpireAt(relation.getExpireAt());
        log.setReason(reason);
        log.setOperatorId(operatorId == null && UserContext.getCurrentUser() != null
                ? UserContext.getCurrentUser().getId() : operatorId);
        log.setOperatorType(operatorId == null ? "SYSTEM" : "ADMIN");
        relationLogMapper.insert(log);
    }

    /**
     * 查询指定关系的历史变更日志列表。
     *
     * @param relationLogMapper 日志持久层映射
     * @param memberService 会员服务
     * @param distributionMemberSupport 分销员支持组件
     * @param relationId 关系ID
     * @return 展示 VO 列表
     */
    public static List<DistributionCustomerRelationLogVO> listRelationLogs(DistributionCustomerRelationLogMapper relationLogMapper,
                                                                           MemberService memberService,
                                                                           DistributionMemberSupport distributionMemberSupport,
                                                                           String relationId) {
        List<DistributionCustomerRelationLog> logs = relationLogMapper.selectList(
                new LambdaQueryWrapper<DistributionCustomerRelationLog>()
                        .eq(DistributionCustomerRelationLog::getRelationId, relationId)
                        .in(DistributionCustomerRelationLog::getChangeType,
                                DistributionCustomerRelationChangeTypeEnum.CREATE.getCode(),
                                DistributionCustomerRelationChangeTypeEnum.EXPIRE.getCode(),
                                DistributionCustomerRelationChangeTypeEnum.ADMIN_REBIND.getCode(),
                                DistributionCustomerRelationChangeTypeEnum.ADMIN_UNBIND.getCode())
                        .orderByDesc(DistributionCustomerRelationLog::getCreateTime));
        return logs.stream().map(log -> toLogVO(log, memberService, distributionMemberSupport)).toList();
    }

    /**
     * 将关系变更日志转为展示 VO。
     */
    public static DistributionCustomerRelationLogVO toLogVO(DistributionCustomerRelationLog log,
                                                            MemberService memberService,
                                                            DistributionMemberSupport distributionMemberSupport) {
        DistributionCustomerRelationLogVO vo = new DistributionCustomerRelationLogVO();
        vo.setId(log.getId());
        Member member = memberService.getById(log.getMemberId());
        if (member != null) {
            vo.setMemberNickname(member.getNickName());
            vo.setMemberMobile(member.getMobile());
        }
        vo.setChangeType(log.getChangeType());
        vo.setChangeTypeLabel(resolveChangeTypeLabel(log));
        vo.setBeforeDistributionId(log.getBeforeDistributionId());
        vo.setBeforeDistributionNickname(resolveDistributionNickname(distributionMemberSupport, log.getBeforeDistributionId()));
        vo.setBeforeDistributionMobile(resolveDistributionMobile(distributionMemberSupport, memberService, log.getBeforeDistributionId()));
        vo.setAfterDistributionId(log.getAfterDistributionId());
        vo.setAfterDistributionNickname(resolveDistributionNickname(distributionMemberSupport, log.getAfterDistributionId()));
        vo.setAfterDistributionMobile(resolveDistributionMobile(distributionMemberSupport, memberService, log.getAfterDistributionId()));
        vo.setBeforeRelationStatus(log.getBeforeRelationStatus());
        vo.setBeforeRelationStatusLabel(resolveRelationStatusLabel(log.getBeforeRelationStatus()));
        vo.setAfterRelationStatus(log.getAfterRelationStatus());
        vo.setAfterRelationStatusLabel(resolveRelationStatusLabel(log.getAfterRelationStatus()));
        vo.setBeforeExpireAt(log.getBeforeExpireAt());
        vo.setAfterExpireAt(log.getAfterExpireAt());
        vo.setReason(formatLogReason(log));
        vo.setChangeSummary(buildChangeSummary(distributionMemberSupport, log));
        vo.setOperatorType(log.getOperatorType());
        vo.setOperatorTypeLabel(resolveOperatorTypeLabel(log.getOperatorType()));
        vo.setCreateTime(log.getCreateTime());
        return vo;
    }

    /**
     * 根据分销员ID解析分销员手机号。
     */
    public static String resolveDistributionMobile(DistributionMemberSupport distributionMemberSupport,
                                                   MemberService memberService,
                                                   String distributionId) {
        if (distributionId == null || distributionId.isBlank()) {
            return null;
        }
        Distribution distribution = distributionMemberSupport.getById(distributionId);
        if (distribution == null || distribution.getMemberId() == null) {
            return null;
        }
        Member member = memberService.getById(distribution.getMemberId());
        return member == null ? null : member.getMobile();
    }

    /**
     * 根据分销员ID解析分销员昵称。
     */
    public static String resolveDistributionNickname(DistributionMemberSupport distributionMemberSupport,
                                                     String distributionId) {
        if (distributionId == null || distributionId.isBlank()) {
            return "-";
        }
        Distribution distribution = distributionMemberSupport.getById(distributionId);
        if (distribution == null || distribution.getMemberName() == null || distribution.getMemberName().isBlank()) {
            return distributionId;
        }
        return distribution.getMemberName();
    }

    /**
     * 解析变更类型展示文案。
     */
    public static String resolveChangeTypeLabel(DistributionCustomerRelationLog log) {
        if (log == null || log.getChangeType() == null) {
            return "-";
        }
        if (DistributionCustomerRelationChangeTypeEnum.ADMIN_REBIND.getCode().equals(log.getChangeType())
                && (log.getBeforeDistributionId() == null || log.getBeforeDistributionId().isBlank())) {
            return DistributionCustomerRelationChangeTypeEnum.CREATE.getLabel();
        }
        return DistributionCustomerRelationChangeTypeEnum.labelOf(log.getChangeType());
    }

    /**
     * 构建变更摘要说明。
     */
    public static String buildChangeSummary(DistributionMemberSupport distributionMemberSupport,
                                            DistributionCustomerRelationLog log) {
        if (log == null || log.getChangeType() == null) {
            return "-";
        }
        String beforeName = resolveDistributionNickname(distributionMemberSupport, log.getBeforeDistributionId());
        String afterName = resolveDistributionNickname(distributionMemberSupport, log.getAfterDistributionId());
        String reason = formatLogReason(log);
        String reasonSuffix = reason == null || "-".equals(reason) ? "" : "，" + reason;
        // 后台首次绑定展示为「绑定」，换绑展示为「由 A 换绑至 B」
        if (DistributionCustomerRelationChangeTypeEnum.ADMIN_REBIND.getCode().equals(log.getChangeType())
                && (log.getBeforeDistributionId() == null || log.getBeforeDistributionId().isBlank())) {
            return "绑定分销员「" + afterName + "」" + reasonSuffix;
        }
        return switch (log.getChangeType()) {
            case "CREATE" -> "绑定分销员「" + afterName + "」" + reasonSuffix;
            case "EXPIRE" -> "客户关系失效" + reasonSuffix;
            case "ADMIN_REBIND" -> "由「" + beforeName + "」换绑至「" + afterName + "」" + reasonSuffix;
            case "ADMIN_UNBIND" -> "解除与「" + beforeName + "」的绑定" + reasonSuffix;
            default -> "-";
        };
    }

    /**
     * 解析操作人类型展示文案。
     */
    public static String resolveOperatorTypeLabel(String operatorType) {
        if (operatorType == null || operatorType.isBlank()) {
            return "-";
        }
        return switch (operatorType) {
            case "SYSTEM" -> "系统";
            case "ADMIN" -> "后台";
            default -> operatorType;
        };
    }

    /**
     * 解析关系状态展示文案。
     */
    public static String resolveRelationStatusLabel(String relationStatus) {
        if (relationStatus == null || relationStatus.isBlank()) {
            return "-";
        }
        return switch (relationStatus) {
            case "PENDING" -> "待支付转永久";
            case "TEMPORARY" -> "短期有效";
            case "PERMANENT" -> "永久有效";
            case "EXPIRED" -> "已到期";
            case "UNBOUND" -> "已解绑";
            case "SELF_BIND" -> "自绑定";
            default -> relationStatus;
        };
    }

    /**
     * 格式化日志原因字段为可读文案。
     */
    public static String formatLogReason(DistributionCustomerRelationLog log) {
        if (log.getReason() == null || log.getReason().isBlank()) {
            return switch (log.getChangeType() == null ? "" : log.getChangeType()) {
                case "EXPIRE" -> "客户关系失效";
                default -> "-";
            };
        }
        return switch (log.getReason()) {
            case "TEMPORARY relation expired" -> "短期关系已到期";
            case "PENDING deadline reached" -> "待转永久期限已到";
            case "SCAN" -> "来源：扫码绑定";
            case "ADMIN" -> "来源：后台操作";
            case "ORDER" -> "来源：订单触发";
            case "SELF_BIND" -> "来源：自绑定";
            default -> log.getReason();
        };
    }

    /**
     * 根据变更日志解析失效原因。
     */
    public static String resolveInvalidReason(DistributionCustomerRelationLog log) {
        if (log.getReason() != null && !log.getReason().isBlank()) {
            if ("TEMPORARY relation expired".equals(log.getReason())) {
                return "短期关系已到期";
            }
            if ("PENDING deadline reached".equals(log.getReason())) {
                return "待转永久期限已到";
            }
            return log.getReason();
        }
        if ("EXPIRE".equals(log.getChangeType())) {
            return "客户关系失效";
        }
        if ("ADMIN_UNBIND".equals(log.getChangeType())) {
            return "后台解绑";
        }
        return "-";
    }

    /**
     * 根据关系状态解析失效原因（无日志时的兜底）。
     */
    public static String resolveInvalidReasonByStatus(String relationStatus) {
        if (DistributionRelationStatusEnum.EXPIRED.name().equals(relationStatus)) {
            return "客户关系失效";
        }
        if (DistributionRelationStatusEnum.UNBOUND.name().equals(relationStatus)) {
            return "已解绑";
        }
        return "-";
    }
}
