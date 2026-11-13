package cn.lili.modules.distribution.support;

import cn.hutool.core.date.DateUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.entity.enums.DistributionBindModeEnum;
import cn.lili.modules.distribution.entity.enums.DistributionCustomerRelationChangeTypeEnum;
import cn.lili.modules.distribution.entity.enums.DistributionPermanentBindModeEnum;
import cn.lili.modules.distribution.entity.enums.DistributionRelationStatusEnum;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.mapper.DistributionCustomerRelationLogMapper;
import cn.lili.modules.distribution.mapper.DistributionMapper;
import cn.lili.modules.distribution.serviceimpl.DistributionCustomerRelationServiceImpl;
import cn.lili.modules.system.entity.dto.DistributionCustomerSetting;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;

import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * 分销客户绑定关系（扫码绑定、支付转永久、后台重绑/解绑与超期失效）支持类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
public class DistributionCustomerRelationBindSupport {

    public static final List<String> ACTIVE_STATUSES = List.of(
            DistributionRelationStatusEnum.PENDING.name(),
            DistributionRelationStatusEnum.TEMPORARY.name(),
            DistributionRelationStatusEnum.PERMANENT.name(),
            DistributionRelationStatusEnum.SELF_BIND.name()
    );

    /**
     * 买家扫码绑定分销员关系。
     */
    public static void bindOnScan(DistributionCustomerRelationServiceImpl service,
                                  DistributionMemberSupport distributionMemberSupport,
                                  DistributionCustomerRelationLogMapper relationLogMapper,
                                  DistributionMapper distributionMapper,
                                  Cache<String> cache,
                                  String distributionId,
                                  String memberId,
                                  String source) {
        // 校验目标分销员存在且已通过审核
        Distribution distribution = distributionMemberSupport.getById(distributionId);
        if (distribution == null || !DistributionStatusEnum.PASS.name().equals(distribution.getDistributionStatus())) {
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }
        // 扫码人是分销员本人 → 仅记录自绑定，不参与客户归属
        if (distribution.getMemberId().equals(memberId)) {
            saveSelfBind(service, relationLogMapper, distributionMapper, memberId, distributionId, source);
            return;
        }
        DistributionCustomerSetting setting = service.getCustomerSetting();
        Date now = new Date();
        DistributionCustomerRelation relation = service.getOne(new LambdaQueryWrapper<DistributionCustomerRelation>()
                .eq(DistributionCustomerRelation::getMemberId, memberId));

        // 已有受保护关系：同一分销员可续期短期绑定；绑定他人则直接忽略
        if (relation != null && isProtectedRelation(relation, now)) {
            if (!distributionId.equals(relation.getDistributionId())) {
                return;
            }
            if (DistributionRelationStatusEnum.TEMPORARY.name().equals(relation.getRelationStatus())) {
                int days = setting.getShortTermBindDays() == null ? 7 : setting.getShortTermBindDays();
                relation.setLastScanTime(now);
                relation.setExpireAt(DateUtil.offsetDay(now, days));
                service.updateById(relation);
                cacheRelation(cache, memberId, distributionId, setting);
            }
            return;
        }

        // 新建或换绑：按配置写入关系状态（短期 / 永久 / 待支付转永久）
        String beforeDistributionId = relation == null ? null : relation.getDistributionId();
        String beforeStatus = relation == null ? null : relation.getRelationStatus();
        Date beforeExpireAt = relation == null ? null : relation.getExpireAt();

        if (relation == null) {
            relation = new DistributionCustomerRelation();
            relation.setMemberId(memberId);
        }
        relation.setDistributionId(distributionId);
        relation.setBindSource(source);
        relation.setInviteCode(distribution.getInviteCode());
        relation.setBindMode(setting.getBindMode());
        relation.setPermanentBindMode(setting.getPermanentBindMode());
        relation.setBindTime(now);
        relation.setLastScanTime(now);
        applyBindMode(relation, setting, now);
        service.saveOrUpdate(relation);
        DistributionCustomerRelationLogSupport.writeLog(relationLogMapper, relation, "CREATE", beforeDistributionId, beforeStatus, beforeExpireAt, source, null);
        cacheRelation(cache, memberId, distributionId, setting);
        syncValidCustomerCount(distributionMapper, beforeDistributionId);
        syncValidCustomerCount(distributionMapper, distributionId);
    }

    /**
     * 支付完成触发转永久绑定。
     */
    public static void bindPermanentOnPaid(DistributionCustomerRelationServiceImpl service,
                                          DistributionCustomerRelationLogMapper relationLogMapper,
                                          DistributionMapper distributionMapper,
                                          String memberId,
                                          String orderSn) {
        DistributionCustomerRelation relation = service.getOne(new LambdaQueryWrapper<DistributionCustomerRelation>()
                .eq(DistributionCustomerRelation::getMemberId, memberId));
        if (relation == null) {
            return;
        }
        // 仅「待支付转永久」且在期限内可由支付订单触发
        if (!DistributionRelationStatusEnum.PENDING.name().equals(relation.getRelationStatus())) {
            return;
        }
        if (relation.getPendingPermanentDeadline() != null
                && relation.getPendingPermanentDeadline().before(new Date())) {
            return;
        }
        String beforeStatus = relation.getRelationStatus();
        Date beforeExpireAt = relation.getExpireAt();
        Date now = new Date();
        relation.setRelationStatus(DistributionRelationStatusEnum.PERMANENT.name());
        relation.setPermanentAt(now);
        relation.setExpireAt(null);
        relation.setTriggerOrderSn(orderSn);
        service.updateById(relation);
        DistributionCustomerRelationLogSupport.writeLog(relationLogMapper, relation, "TO_PERMANENT", relation.getDistributionId(), beforeStatus, beforeExpireAt, "ORDER", null);
        syncValidCustomerCount(distributionMapper, relation.getDistributionId());
    }

    /**
     * 获取会员当前生效中的客户关系。
     */
    public static DistributionCustomerRelation getActiveRelation(DistributionCustomerRelationServiceImpl service, String memberId) {
        DistributionCustomerRelation relation = service.getOne(new LambdaQueryWrapper<DistributionCustomerRelation>()
                .eq(DistributionCustomerRelation::getMemberId, memberId));
        // 非有效状态或已过到期时间视为无绑定
        if (relation == null || !ACTIVE_STATUSES.contains(relation.getRelationStatus())) {
            return null;
        }
        if (relation.getExpireAt() != null && relation.getExpireAt().before(new Date())) {
            return null;
        }
        return relation;
    }

    /**
     * 管理端手动为会员重新绑定分销员。
     */
    public static void adminRebind(DistributionCustomerRelationServiceImpl service,
                                   DistributionCustomerRelationLogMapper relationLogMapper,
                                   DistributionMapper distributionMapper,
                                   Cache<String> cache,
                                   String memberId,
                                   String distributionId,
                                   String reason,
                                   String operatorId) {
        DistributionCustomerRelation relation = service.getOne(new LambdaQueryWrapper<DistributionCustomerRelation>()
                .eq(DistributionCustomerRelation::getMemberId, memberId));
        String beforeDistributionId = relation == null ? null : relation.getDistributionId();
        String beforeStatus = relation == null ? null : relation.getRelationStatus();
        Date beforeExpireAt = relation == null ? null : relation.getExpireAt();
        DistributionCustomerSetting setting = service.getCustomerSetting();
        Date now = new Date();
        if (relation == null) {
            relation = new DistributionCustomerRelation();
            relation.setMemberId(memberId);
        }
        // 后台换绑直接设为永久有效
        relation.setDistributionId(distributionId);
        relation.setBindSource("ADMIN");
        relation.setBindTime(now);
        relation.setLastScanTime(now);
        relation.setBindMode(setting.getBindMode());
        relation.setPermanentBindMode(setting.getPermanentBindMode());
        relation.setRelationStatus(DistributionRelationStatusEnum.PERMANENT.name());
        relation.setPermanentAt(now);
        relation.setExpireAt(null);
        service.saveOrUpdate(relation);
        DistributionCustomerRelationLogSupport.writeLog(relationLogMapper, relation, "ADMIN_REBIND", beforeDistributionId, beforeStatus, beforeExpireAt, reason, operatorId);
        cacheRelation(cache, memberId, distributionId, setting);
        syncValidCustomerCount(distributionMapper, beforeDistributionId);
        syncValidCustomerCount(distributionMapper, distributionId);
    }

    /**
     * 管理端手动解绑会员与分销员的关系。
     */
    public static void adminUnbind(DistributionCustomerRelationServiceImpl service,
                                   DistributionCustomerRelationLogMapper relationLogMapper,
                                   DistributionMapper distributionMapper,
                                   Cache<String> cache,
                                   String memberId,
                                   String reason,
                                   String operatorId) {
        DistributionCustomerRelation relation = service.getOne(new LambdaQueryWrapper<DistributionCustomerRelation>()
                .eq(DistributionCustomerRelation::getMemberId, memberId));
        if (relation == null) {
            return;
        }
        String beforeDistributionId = relation.getDistributionId();
        String beforeStatus = relation.getRelationStatus();
        Date beforeExpireAt = relation.getExpireAt();
        // 清空分销员归属并标记为已解绑
        relation.setDistributionId(null);
        relation.setRelationStatus(DistributionRelationStatusEnum.UNBOUND.name());
        relation.setExpireAt(null);
        service.updateById(relation);
        DistributionCustomerRelationLogSupport.writeLog(relationLogMapper, relation, "ADMIN_UNBIND", beforeDistributionId, beforeStatus, beforeExpireAt, reason, operatorId);
        cache.remove(CachePrefix.DISTRIBUTION.getPrefix() + "_" + memberId);
        syncValidCustomerCount(distributionMapper, beforeDistributionId);
    }

    /**
     * 定时扫描并将超期未支付/超期绑定的关系标记失效。
     */
    public static int expirePendingRelations(DistributionCustomerRelationServiceImpl service,
                                             DistributionCustomerRelationLogMapper relationLogMapper,
                                             DistributionMapper distributionMapper,
                                             Cache<String> cache) {
        Date now = new Date();
        // 待转永久超期
        List<DistributionCustomerRelation> expiredPending = service.list(new LambdaQueryWrapper<DistributionCustomerRelation>()
                .eq(DistributionCustomerRelation::getRelationStatus, DistributionRelationStatusEnum.PENDING.name())
                .lt(DistributionCustomerRelation::getPendingPermanentDeadline, now));
        // 短期绑定到期
        List<DistributionCustomerRelation> expiredTemporary = service.list(new LambdaQueryWrapper<DistributionCustomerRelation>()
                .eq(DistributionCustomerRelation::getRelationStatus, DistributionRelationStatusEnum.TEMPORARY.name())
                .lt(DistributionCustomerRelation::getExpireAt, now));
        int count = 0;
        for (DistributionCustomerRelation relation : expiredPending) {
            expireRelation(service, relationLogMapper, distributionMapper, cache, relation, "PENDING deadline reached");
            count++;
        }
        for (DistributionCustomerRelation relation : expiredTemporary) {
            expireRelation(service, relationLogMapper, distributionMapper, cache, relation, "TEMPORARY relation expired");
            count++;
        }
        return count;
    }

    /**
     * 将客户关系标记为已失效并记录日志、刷新缓存与有效客户数。
     */
    public static void expireRelation(DistributionCustomerRelationServiceImpl service,
                                      DistributionCustomerRelationLogMapper relationLogMapper,
                                      DistributionMapper distributionMapper,
                                      Cache<String> cache,
                                      DistributionCustomerRelation relation,
                                      String reason) {
        String beforeStatus = relation.getRelationStatus();
        Date beforeExpireAt = relation.getExpireAt();
        relation.setRelationStatus(DistributionRelationStatusEnum.EXPIRED.name());
        service.updateById(relation);
        DistributionCustomerRelationLogSupport.writeLog(relationLogMapper, relation, "EXPIRE", relation.getDistributionId(), beforeStatus, beforeExpireAt, reason, null);
        cache.remove(CachePrefix.DISTRIBUTION.getPrefix() + "_" + relation.getMemberId());
        syncValidCustomerCount(distributionMapper, relation.getDistributionId());
    }

    /**
     * 保存分销员自绑定关系（扫码绑定自身）。
     */
    public static void saveSelfBind(DistributionCustomerRelationServiceImpl service,
                                    DistributionCustomerRelationLogMapper relationLogMapper,
                                    DistributionMapper distributionMapper,
                                    String memberId,
                                    String distributionId,
                                    String source) {
        DistributionCustomerRelation relation = service.getOne(new LambdaQueryWrapper<DistributionCustomerRelation>()
                .eq(DistributionCustomerRelation::getMemberId, memberId));
        String beforeDistributionId = relation == null ? null : relation.getDistributionId();
        String beforeStatus = relation == null ? null : relation.getRelationStatus();
        Date beforeExpireAt = relation == null ? null : relation.getExpireAt();
        if (relation == null) {
            relation = new DistributionCustomerRelation();
            relation.setMemberId(memberId);
        }
        relation.setDistributionId(distributionId);
        relation.setBindSource(source);
        relation.setRelationStatus(DistributionRelationStatusEnum.SELF_BIND.name());
        relation.setBindTime(new Date());
        service.saveOrUpdate(relation);
        DistributionCustomerRelationLogSupport.writeLog(relationLogMapper, relation,
                DistributionCustomerRelationChangeTypeEnum.CREATE.getCode(),
                beforeDistributionId, beforeStatus, beforeExpireAt, source, null);
        syncValidCustomerCount(distributionMapper, beforeDistributionId);
        syncValidCustomerCount(distributionMapper, distributionId);
    }

    /**
     * 同步分销员有效客户数量。
     */
    public static void syncValidCustomerCount(DistributionMapper distributionMapper, String distributionId) {
        if (StringUtils.isEmpty(distributionId)) {
            return;
        }
        distributionMapper.syncValidCustomerCount(distributionId);
    }

    /**
     * 按客户绑定配置设置关系状态及到期时间。
     */
    public static void applyBindMode(DistributionCustomerRelation relation, DistributionCustomerSetting setting, Date now) {
        if (DistributionBindModeEnum.PERMANENT.name().equals(setting.getBindMode())) {
            // 永久绑定：扫码即永久，或支付后 N 天内转永久
            if (DistributionPermanentBindModeEnum.SCAN_IMMEDIATE.name().equals(setting.getPermanentBindMode())) {
                relation.setRelationStatus(DistributionRelationStatusEnum.PERMANENT.name());
                relation.setPermanentAt(now);
                relation.setExpireAt(null);
            } else {
                relation.setRelationStatus(DistributionRelationStatusEnum.PENDING.name());
                int days = setting.getPermanentPaidDays() == null || setting.getPermanentPaidDays() < 1
                        ? 1 : setting.getPermanentPaidDays();
                relation.setPendingPermanentDeadline(DateUtil.offsetDay(now, days));
            }
            return;
        }
        // 短期绑定：按配置天数设置到期时间
        relation.setRelationStatus(DistributionRelationStatusEnum.TEMPORARY.name());
        int days = setting.getShortTermBindDays() == null ? 7 : setting.getShortTermBindDays();
        relation.setExpireAt(DateUtil.offsetDay(now, days));
    }

    /**
     * 将会员与分销员关系写入缓存。
     */
    public static void cacheRelation(Cache<String> cache, String memberId, String distributionId, DistributionCustomerSetting setting) {
        int days = setting.getShortTermBindDays() == null ? 7 : setting.getShortTermBindDays();
        cache.put(CachePrefix.DISTRIBUTION.getPrefix() + "_" + memberId, distributionId, (long) days, TimeUnit.DAYS);
    }

    /**
     * 返回默认客户绑定配置。
     */
    public static DistributionCustomerSetting defaultCustomerSetting() {
        DistributionCustomerSetting setting = new DistributionCustomerSetting();
        setting.setBindMode(DistributionBindModeEnum.SHORT_TERM.name());
        setting.setPermanentBindMode(DistributionPermanentBindModeEnum.PAID_WITHIN_DAYS.name());
        setting.setPermanentPaidDays(1);
        setting.setShortTermBindDays(7);
        return setting;
    }

    /**
     * 判断客户关系是否处于保护期内（有效且未过期）。
     */
    public static boolean isProtectedRelation(DistributionCustomerRelation relation, Date now) {
        if (relation == null || !ACTIVE_STATUSES.contains(relation.getRelationStatus())) {
            return false;
        }
        // 短期到期或待转永久超期后不再受保护
        if (relation.getExpireAt() != null && relation.getExpireAt().before(now)) {
            return false;
        }
        if (DistributionRelationStatusEnum.PENDING.name().equals(relation.getRelationStatus())
                && relation.getPendingPermanentDeadline() != null
                && relation.getPendingPermanentDeadline().before(now)) {
            return false;
        }
        return true;
    }
}
