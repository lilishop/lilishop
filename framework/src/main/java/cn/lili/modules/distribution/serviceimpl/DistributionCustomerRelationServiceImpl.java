package cn.lili.modules.distribution.serviceimpl;

import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.StringUtils;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelationLog;
import cn.lili.modules.distribution.entity.dto.DistributionBuyerCustomerQueryDTO;
import cn.lili.modules.distribution.entity.dto.DistributionCustomerRelationSearchParams;
import cn.lili.modules.distribution.entity.enums.DistributionCustomerRelationChangeTypeEnum;
import cn.lili.modules.distribution.entity.enums.DistributionBindModeEnum;
import cn.lili.modules.distribution.entity.enums.DistributionPermanentBindModeEnum;
import cn.lili.modules.distribution.entity.enums.DistributionRelationStatusEnum;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerPageVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerTradeStatsVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerVO;
import cn.lili.modules.distribution.entity.vos.DistributionCustomerRelationDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionCustomerRelationLogVO;
import cn.lili.modules.distribution.entity.vos.DistributionCustomerRelationVO;
import cn.lili.modules.distribution.mapper.DistributionCustomerRelationLogMapper;
import cn.lili.modules.distribution.mapper.DistributionCustomerRelationMapper;
import cn.lili.modules.distribution.mapper.DistributionMapper;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.support.DistributionInviteRangeSupport;
import cn.lili.modules.distribution.support.DistributionMemberSupport;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.DistributionCustomerSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * 分销客户关系服务实现
 *
 * @author pikachu
 */
@Service
public class DistributionCustomerRelationServiceImpl
        extends ServiceImpl<DistributionCustomerRelationMapper, DistributionCustomerRelation>
        implements DistributionCustomerRelationService {

    private static final List<String> ACTIVE_STATUSES = List.of(
            DistributionRelationStatusEnum.PENDING.name(),
            DistributionRelationStatusEnum.TEMPORARY.name(),
            DistributionRelationStatusEnum.PERMANENT.name(),
            DistributionRelationStatusEnum.SELF_BIND.name()
    );

    @Autowired
    private SettingService settingService;

    @Autowired
    private DistributionCustomerRelationLogMapper relationLogMapper;

    @Autowired
    private DistributionMapper distributionMapper;

    @Autowired
    private DistributionMemberSupport distributionMemberSupport;

    @Autowired
    private MemberService memberService;

    @Autowired
    @SuppressWarnings("unchecked")
    private Cache<String> cache;

    @Override
    public DistributionCustomerSetting getCustomerSetting() {
        Setting setting = settingService.get(SettingEnum.DISTRIBUTION_CUSTOMER_SETTING.name());
        if (setting == null || setting.getSettingValue() == null) {
            return defaultCustomerSetting();
        }
        return JSONUtil.toBean(setting.getSettingValue(), DistributionCustomerSetting.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void bindOnScan(String distributionId, String memberId, String source) {
        // 校验目标分销员存在且已通过审核
        Distribution distribution = distributionMemberSupport.getById(distributionId);
        if (distribution == null || !DistributionStatusEnum.PASS.name().equals(distribution.getDistributionStatus())) {
            throw new ServiceException(ResultCode.DISTRIBUTION_NOT_EXIST);
        }
        // 扫码人是分销员本人 → 仅记录自绑定，不参与客户归属
        if (distribution.getMemberId().equals(memberId)) {
            saveSelfBind(memberId, distributionId, source);
            return;
        }
        DistributionCustomerSetting setting = getCustomerSetting();
        Date now = new Date();
        DistributionCustomerRelation relation = getOne(new LambdaQueryWrapper<DistributionCustomerRelation>()
                .eq(DistributionCustomerRelation::getMemberId, memberId));

        // 已有受保护关系：同一分销员可续期短期绑定；绑定他人则直接忽略
        if (relation != null && isProtectedRelation(relation, now)) {
            if (!distributionId.equals(relation.getDistributionId())) {
                return;
            }
            if (DistributionRelationStatusEnum.TEMPORARY.name().equals(relation.getRelationStatus())) {
                int days = setting.getShortTermBindDays() == null ? 7 : setting.getShortTermBindDays();
                relation.setLastScanTime(now);
                relation.setExpireAt(cn.hutool.core.date.DateUtil.offsetDay(now, days));
                updateById(relation);
                cacheRelation(memberId, distributionId, setting);
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
        saveOrUpdate(relation);
        writeLog(relation, "CREATE", beforeDistributionId, beforeStatus, beforeExpireAt, source, null);
        cacheRelation(memberId, distributionId, setting);
        syncValidCustomerCount(beforeDistributionId);
        syncValidCustomerCount(distributionId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void bindPermanentOnPaid(String memberId, String orderSn) {
        DistributionCustomerRelation relation = getOne(new LambdaQueryWrapper<DistributionCustomerRelation>()
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
        updateById(relation);
        writeLog(relation, "TO_PERMANENT", relation.getDistributionId(), beforeStatus, beforeExpireAt, "ORDER", null);
        syncValidCustomerCount(relation.getDistributionId());
    }

    @Override
    public DistributionCustomerRelation getActiveRelation(String memberId) {
        DistributionCustomerRelation relation = getOne(new LambdaQueryWrapper<DistributionCustomerRelation>()
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

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void adminRebind(String memberId, String distributionId, String reason, String operatorId) {
        DistributionCustomerRelation relation = getOne(new LambdaQueryWrapper<DistributionCustomerRelation>()
                .eq(DistributionCustomerRelation::getMemberId, memberId));
        String beforeDistributionId = relation == null ? null : relation.getDistributionId();
        String beforeStatus = relation == null ? null : relation.getRelationStatus();
        Date beforeExpireAt = relation == null ? null : relation.getExpireAt();
        DistributionCustomerSetting setting = getCustomerSetting();
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
        saveOrUpdate(relation);
        writeLog(relation, "ADMIN_REBIND", beforeDistributionId, beforeStatus, beforeExpireAt, reason, operatorId);
        cacheRelation(memberId, distributionId, setting);
        syncValidCustomerCount(beforeDistributionId);
        syncValidCustomerCount(distributionId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void adminUnbind(String memberId, String reason, String operatorId) {
        DistributionCustomerRelation relation = getOne(new LambdaQueryWrapper<DistributionCustomerRelation>()
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
        updateById(relation);
        writeLog(relation, "ADMIN_UNBIND", beforeDistributionId, beforeStatus, beforeExpireAt, reason, operatorId);
        cache.remove(CachePrefix.DISTRIBUTION.getPrefix() + "_" + memberId);
        syncValidCustomerCount(beforeDistributionId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int expirePendingRelations() {
        Date now = new Date();
        // 待转永久超期
        List<DistributionCustomerRelation> expiredPending = list(new LambdaQueryWrapper<DistributionCustomerRelation>()
                .eq(DistributionCustomerRelation::getRelationStatus, DistributionRelationStatusEnum.PENDING.name())
                .lt(DistributionCustomerRelation::getPendingPermanentDeadline, now));
        // 短期绑定到期
        List<DistributionCustomerRelation> expiredTemporary = list(new LambdaQueryWrapper<DistributionCustomerRelation>()
                .eq(DistributionCustomerRelation::getRelationStatus, DistributionRelationStatusEnum.TEMPORARY.name())
                .lt(DistributionCustomerRelation::getExpireAt, now));
        int count = 0;
        for (DistributionCustomerRelation relation : expiredPending) {
            expireRelation(relation, "PENDING deadline reached");
            count++;
        }
        for (DistributionCustomerRelation relation : expiredTemporary) {
            expireRelation(relation, "TEMPORARY relation expired");
            count++;
        }
        return count;
    }

    @Override
    public IPage<DistributionCustomerRelationVO> relationPage(DistributionCustomerRelationSearchParams params, PageVO page) {
        LambdaQueryWrapper<DistributionCustomerRelation> queryWrapper = params.queryWrapper();
        // 按客户手机号反查会员 ID
        if (StringUtils.isNotEmpty(params.getMemberMobile())) {
            List<String> memberIds = memberService.list(new LambdaQueryWrapper<Member>()
                            .like(Member::getMobile, params.getMemberMobile()))
                    .stream()
                    .map(Member::getId)
                    .collect(Collectors.toList());
            if (memberIds.isEmpty()) {
                return emptyRelationPage(page);
            }
            queryWrapper.in(DistributionCustomerRelation::getMemberId, memberIds);
        }
        // 按分销员手机号反查分销员 ID
        if (StringUtils.isNotEmpty(params.getDistributionMobile())) {
            List<String> distributorMemberIds = memberService.list(new LambdaQueryWrapper<Member>()
                            .like(Member::getMobile, params.getDistributionMobile()))
                    .stream()
                    .map(Member::getId)
                    .collect(Collectors.toList());
            if (distributorMemberIds.isEmpty()) {
                return emptyRelationPage(page);
            }
            List<String> distributionIds = distributionMapper.selectList(new LambdaQueryWrapper<Distribution>()
                            .in(Distribution::getMemberId, distributorMemberIds))
                    .stream()
                    .map(Distribution::getId)
                    .collect(Collectors.toList());
            if (distributionIds.isEmpty()) {
                return emptyRelationPage(page);
            }
            queryWrapper.in(DistributionCustomerRelation::getDistributionId, distributionIds);
        }
        // 绑定时间范围
        Date bindTimeStart = params.getBindTimeStart();
        Date bindTimeEnd = params.getBindTimeEnd();
        if (bindTimeStart != null && bindTimeEnd != null) {
            queryWrapper.between(DistributionCustomerRelation::getBindTime, bindTimeStart, DateUtil.endOfDate(bindTimeEnd));
        } else if (bindTimeStart != null) {
            queryWrapper.ge(DistributionCustomerRelation::getBindTime, bindTimeStart);
        } else if (bindTimeEnd != null) {
            queryWrapper.le(DistributionCustomerRelation::getBindTime, DateUtil.endOfDate(bindTimeEnd));
        }
        // 是否有效关系
        if (params.getRelationEffective() != null) {
            if (Boolean.TRUE.equals(params.getRelationEffective())) {
                queryWrapper.notIn(DistributionCustomerRelation::getRelationStatus,
                        DistributionRelationStatusEnum.EXPIRED.name(),
                        DistributionRelationStatusEnum.UNBOUND.name());
            } else {
                queryWrapper.in(DistributionCustomerRelation::getRelationStatus,
                        DistributionRelationStatusEnum.EXPIRED.name(),
                        DistributionRelationStatusEnum.UNBOUND.name());
            }
        }
        // 是否有到期时间
        if (params.getHasDeadline() != null) {
            if (Boolean.TRUE.equals(params.getHasDeadline())) {
                queryWrapper.isNotNull(DistributionCustomerRelation::getExpireAt);
            } else {
                queryWrapper.isNull(DistributionCustomerRelation::getExpireAt);
            }
        }
        IPage<DistributionCustomerRelation> relationPage =
                this.page(PageUtil.<DistributionCustomerRelation>initPage(page), queryWrapper);
        Page<DistributionCustomerRelationVO> result = new Page<>(relationPage.getCurrent(), relationPage.getSize(), relationPage.getTotal());
        result.setRecords(relationPage.getRecords().stream().map(this::toVO).toList());
        return result;
    }

    @Override
    public DistributionCustomerRelationDetailVO getRelationDetail(String id) {
        DistributionCustomerRelation relation = getById(id);
        if (relation == null) {
            throw new ServiceException(ResultCode.ERROR);
        }
        return toDetailVO(relation);
    }

    @Override
    public List<DistributionCustomerRelationLogVO> listRelationLogs(String relationId) {
        List<DistributionCustomerRelationLog> logs = relationLogMapper.selectList(
                new LambdaQueryWrapper<DistributionCustomerRelationLog>()
                        .eq(DistributionCustomerRelationLog::getRelationId, relationId)
                        .in(DistributionCustomerRelationLog::getChangeType,
                                DistributionCustomerRelationChangeTypeEnum.CREATE.getCode(),
                                DistributionCustomerRelationChangeTypeEnum.EXPIRE.getCode(),
                                DistributionCustomerRelationChangeTypeEnum.ADMIN_REBIND.getCode(),
                                DistributionCustomerRelationChangeTypeEnum.ADMIN_UNBIND.getCode())
                        .orderByDesc(DistributionCustomerRelationLog::getCreateTime));
        return logs.stream().map(this::toLogVO).toList();
    }

    @Override
    public DistributionBuyerCustomerPageVO pageBuyerCustomers(String distributionId, DistributionBuyerCustomerQueryDTO query) {
        // 解析分页、筛选类型、关键词与时间范围
        DistributionBuyerCustomerQueryDTO safeQuery = query == null ? new DistributionBuyerCustomerQueryDTO() : query;
        long current = safeQuery.getPageNumber() == null || safeQuery.getPageNumber() <= 0 ? 1 : safeQuery.getPageNumber();
        long size = safeQuery.getPageSize() == null || safeQuery.getPageSize() <= 0 ? 10 : safeQuery.getPageSize();
        String filterType = StringUtils.isBlank(safeQuery.getFilterType()) ? "ALL" : safeQuery.getFilterType().trim().toUpperCase();
        String keyword = StringUtils.isBlank(safeQuery.getKeyword()) ? null : safeQuery.getKeyword().trim();
        DistributionInviteRangeSupport.DateRange range =
                DistributionInviteRangeSupport.resolveRange(safeQuery.getRangeType(), safeQuery.getStartTime(), safeQuery.getEndTime());

        DistributionBuyerCustomerPageVO pageVO = new DistributionBuyerCustomerPageVO();
        pageVO.setCurrent(current);
        pageVO.setSize(size);
        if (StringUtils.isBlank(distributionId)) {
            return pageVO;
        }
        long total = baseMapper.countBuyerCustomers(distributionId, filterType, keyword, range.getStartTime(), range.getEndTime());
        pageVO.setTotal(total);
        if (total <= 0) {
            return pageVO;
        }
        long offset = (current - 1) * size;
        List<DistributionBuyerCustomerVO> records =
                baseMapper.listBuyerCustomers(distributionId, filterType, keyword, range.getStartTime(), range.getEndTime(), offset, size);
        // 补全关系有效性、状态文案等展示字段
        for (DistributionBuyerCustomerVO record : records) {
            fillBuyerCustomerDisplay(record);
        }
        pageVO.setRecords(records);
        return pageVO;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateBuyerCustomerStarred(String distributionId, String relationId, Boolean starred) {
        if (StringUtils.isBlank(distributionId) || StringUtils.isBlank(relationId) || starred == null) {
            throw new ServiceException(ResultCode.ERROR);
        }
        DistributionCustomerRelation relation = getById(relationId);
        if (relation == null || !distributionId.equals(relation.getDistributionId())) {
            throw new ServiceException(ResultCode.ERROR);
        }
        relation.setStarred(Boolean.TRUE.equals(starred));
        updateById(relation);
    }

    @Override
    public DistributionBuyerCustomerDetailVO getBuyerCustomerDetail(String distributionId, String relationId) {
        if (StringUtils.isBlank(distributionId) || StringUtils.isBlank(relationId)) {
            throw new ServiceException(ResultCode.ERROR);
        }
        DistributionCustomerRelation relation = getById(relationId);
        if (relation == null || !distributionId.equals(relation.getDistributionId())) {
            throw new ServiceException(ResultCode.ERROR);
        }
        DistributionBuyerCustomerDetailVO detail = new DistributionBuyerCustomerDetailVO();
        detail.setId(relation.getId());
        detail.setMemberId(relation.getMemberId());
        detail.setStarred(Boolean.TRUE.equals(relation.getStarred()));

        // 会员基础信息
        Member member = memberService.getById(relation.getMemberId());
        if (member != null) {
            String nickname = StringUtils.isNotBlank(member.getNickName()) ? member.getNickName() : member.getUsername();
            detail.setMemberNickname(StringUtils.isNotBlank(nickname) ? nickname : relation.getMemberId());
            detail.setMemberAvatar(member.getFace());
            detail.setMemberMobile(member.getMobile());
        } else {
            detail.setMemberNickname(relation.getMemberId());
        }

        // 该客户在本分销员下的推广成交统计
        DistributionBuyerCustomerTradeStatsVO stats =
                baseMapper.selectBuyerCustomerTradeStats(distributionId, relation.getMemberId());
        long orderCount = stats == null || stats.getOrderCount() == null ? 0L : stats.getOrderCount();
        BigDecimal tradeAmount = stats == null || stats.getTradeAmount() == null ? BigDecimal.ZERO : stats.getTradeAmount();
        detail.setOrderCount(orderCount);
        detail.setTradeAmount(tradeAmount);
        detail.setLastTradeTime(stats == null ? null : stats.getLastTradeTime());
        if (orderCount > 0) {
            detail.setAverageOrderAmount(tradeAmount.divide(BigDecimal.valueOf(orderCount), 2, RoundingMode.HALF_UP));
        } else {
            detail.setAverageOrderAmount(BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));
        }
        return detail;
    }

    /**
     * 构建空分页结果。
     *
     * @param page 分页参数
     * @return 空分页对象
     */
    private Page<DistributionCustomerRelationVO> emptyRelationPage(PageVO page) {
        Page<DistributionCustomerRelationVO> result = new Page<>(page.getPageNumber(), page.getPageSize(), 0);
        result.setRecords(List.of());
        return result;
    }

    /**
     * 补全买家客户列表展示字段（有效性、状态文案、星标默认值）。
     *
     * @param record 买家客户记录
     */
    private void fillBuyerCustomerDisplay(DistributionBuyerCustomerVO record) {
        if (record == null) {
            return;
        }
        String status = record.getRelationStatus();
        // PENDING / TEMPORARY / PERMANENT 视为有效客户关系
        boolean effective = DistributionRelationStatusEnum.PENDING.name().equals(status)
                || DistributionRelationStatusEnum.TEMPORARY.name().equals(status)
                || DistributionRelationStatusEnum.PERMANENT.name().equals(status);
        record.setRelationEffective(effective);
        if (effective) {
            record.setRelationStatusText("有效");
        } else if (DistributionRelationStatusEnum.EXPIRED.name().equals(status)) {
            record.setRelationStatusText("已失效");
        } else if (DistributionRelationStatusEnum.UNBOUND.name().equals(status)) {
            record.setRelationStatusText("已解绑");
        } else {
            record.setRelationStatusText(resolveRelationStatusLabel(status));
        }
        if (record.getStarred() == null) {
            record.setStarred(Boolean.FALSE);
        }
    }

    /**
     * 将客户关系标记为已失效并记录日志、刷新缓存与有效客户数。
     *
     * @param relation 客户关系
     * @param reason 失效原因
     */
    private void expireRelation(DistributionCustomerRelation relation, String reason) {
        String beforeStatus = relation.getRelationStatus();
        Date beforeExpireAt = relation.getExpireAt();
        relation.setRelationStatus(DistributionRelationStatusEnum.EXPIRED.name());
        updateById(relation);
        writeLog(relation, "EXPIRE", relation.getDistributionId(), beforeStatus, beforeExpireAt, reason, null);
        cache.remove(CachePrefix.DISTRIBUTION.getPrefix() + "_" + relation.getMemberId());
        syncValidCustomerCount(relation.getDistributionId());
    }

    /**
     * 保存分销员自绑定关系（扫码绑定自身）。
     *
     * @param memberId 会员ID
     * @param distributionId 分销员ID
     * @param source 绑定来源
     */
    private void saveSelfBind(String memberId, String distributionId, String source) {
        DistributionCustomerRelation relation = getOne(new LambdaQueryWrapper<DistributionCustomerRelation>()
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
        saveOrUpdate(relation);
        writeLog(relation, DistributionCustomerRelationChangeTypeEnum.CREATE.getCode(),
                beforeDistributionId, beforeStatus, beforeExpireAt, source, null);
        syncValidCustomerCount(beforeDistributionId);
        syncValidCustomerCount(distributionId);
    }

    /**
     * 同步分销员有效客户数量。
     *
     * @param distributionId 分销员ID
     */
    private void syncValidCustomerCount(String distributionId) {
        if (StringUtils.isEmpty(distributionId)) {
            return;
        }
        distributionMapper.syncValidCustomerCount(distributionId);
    }

    /**
     * 按客户绑定配置设置关系状态及到期时间。
     *
     * @param relation 客户关系
     * @param setting 客户绑定配置
     * @param now 当前时间
     */
    private void applyBindMode(DistributionCustomerRelation relation, DistributionCustomerSetting setting, Date now) {
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
                relation.setPendingPermanentDeadline(cn.hutool.core.date.DateUtil.offsetDay(now, days));
            }
            return;
        }
        // 短期绑定：按配置天数设置到期时间
        relation.setRelationStatus(DistributionRelationStatusEnum.TEMPORARY.name());
        int days = setting.getShortTermBindDays() == null ? 7 : setting.getShortTermBindDays();
        relation.setExpireAt(cn.hutool.core.date.DateUtil.offsetDay(now, days));
    }

    /**
     * 将会员与分销员关系写入缓存。
     *
     * @param memberId 会员ID
     * @param distributionId 分销员ID
     * @param setting 客户绑定配置（用于缓存时长）
     */
    private void cacheRelation(String memberId, String distributionId, DistributionCustomerSetting setting) {
        int days = setting.getShortTermBindDays() == null ? 7 : setting.getShortTermBindDays();
        cache.put(CachePrefix.DISTRIBUTION.getPrefix() + "_" + memberId, distributionId, (long) days, TimeUnit.DAYS);
    }

    /**
     * 写入客户关系变更日志。
     *
     * @param relation 当前客户关系
     * @param changeType 变更类型
     * @param beforeDistributionId 变更前分销员ID
     * @param beforeStatus 变更前关系状态
     * @param beforeExpireAt 变更前到期时间
     * @param reason 变更原因
     * @param operatorId 操作人ID（为空时取当前用户或系统）
     */
    private void writeLog(DistributionCustomerRelation relation, String changeType, String beforeDistributionId,
                          String beforeStatus, Date beforeExpireAt, String reason, String operatorId) {
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
     * 将客户关系实体转为列表 VO。
     *
     * @param relation 客户关系实体
     * @return 列表 VO
     */
    private DistributionCustomerRelationVO toVO(DistributionCustomerRelation relation) {
        DistributionCustomerRelationVO vo = new DistributionCustomerRelationVO();
        vo.setId(relation.getId());
        vo.setMemberId(relation.getMemberId());
        vo.setDistributionId(relation.getDistributionId());
        vo.setRelationStatus(relation.getRelationStatus());
        vo.setBindTime(relation.getBindTime());
        vo.setExpireAt(relation.getExpireAt());
        vo.setBindSource(relation.getBindSource());
        vo.setBindMode(relation.getBindMode());
        vo.setHasDeadline(relation.getExpireAt() != null);
        vo.setRelationEffective(isRelationEffective(relation));
        if (!Boolean.TRUE.equals(vo.getRelationEffective())) {
            // 失效关系补充失效时间与原因
            fillInvalidInfo(vo, relation);
        }
        // 客户与分销员展示信息
        Member member = memberService.getById(relation.getMemberId());
        if (member != null) {
            vo.setMemberMobile(member.getMobile());
            vo.setMemberNickname(member.getNickName());
        }
        if (relation.getDistributionId() != null) {
            Distribution distribution = distributionMemberSupport.getById(relation.getDistributionId());
            if (distribution != null) {
                vo.setDistributionNickname(distribution.getMemberName());
                Member distributorMember = memberService.getById(distribution.getMemberId());
                if (distributorMember != null) {
                    vo.setDistributionMobile(distributorMember.getMobile());
                }
            }
        }
        return vo;
    }

    /**
     * 将客户关系实体转为详情 VO。
     *
     * @param relation 客户关系实体
     * @return 详情 VO
     */
    private DistributionCustomerRelationDetailVO toDetailVO(DistributionCustomerRelation relation) {
        // 列表字段复用 toVO，再补充详情专有字段
        DistributionCustomerRelationVO base = toVO(relation);
        DistributionCustomerRelationDetailVO detail = new DistributionCustomerRelationDetailVO();
        detail.setId(base.getId());
        detail.setMemberId(base.getMemberId());
        detail.setMemberMobile(base.getMemberMobile());
        detail.setMemberNickname(base.getMemberNickname());
        detail.setDistributionId(base.getDistributionId());
        detail.setDistributionMobile(base.getDistributionMobile());
        detail.setDistributionNickname(base.getDistributionNickname());
        detail.setRelationStatus(base.getRelationStatus());
        detail.setRelationEffective(base.getRelationEffective());
        detail.setInvalidTime(base.getInvalidTime());
        detail.setInvalidReason(base.getInvalidReason());
        detail.setHasDeadline(base.getHasDeadline());
        detail.setBindTime(base.getBindTime());
        detail.setExpireAt(base.getExpireAt());
        detail.setBindSource(base.getBindSource());
        detail.setBindMode(base.getBindMode());
        detail.setInviteCode(relation.getInviteCode());
        detail.setPermanentBindMode(relation.getPermanentBindMode());
        detail.setLastScanTime(relation.getLastScanTime());
        detail.setPermanentAt(relation.getPermanentAt());
        detail.setPendingPermanentDeadline(relation.getPendingPermanentDeadline());
        detail.setTriggerOrderSn(relation.getTriggerOrderSn());
        detail.setCreateTime(relation.getCreateTime());
        detail.setUpdateTime(relation.getUpdateTime());
        return detail;
    }

    /**
     * 将关系变更日志转为展示 VO。
     *
     * @param log 变更日志
     * @return 日志 VO
     */
    private DistributionCustomerRelationLogVO toLogVO(DistributionCustomerRelationLog log) {
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
        vo.setBeforeDistributionNickname(resolveDistributionNickname(log.getBeforeDistributionId()));
        vo.setBeforeDistributionMobile(resolveDistributionMobile(log.getBeforeDistributionId()));
        vo.setAfterDistributionId(log.getAfterDistributionId());
        vo.setAfterDistributionNickname(resolveDistributionNickname(log.getAfterDistributionId()));
        vo.setAfterDistributionMobile(resolveDistributionMobile(log.getAfterDistributionId()));
        vo.setBeforeRelationStatus(log.getBeforeRelationStatus());
        vo.setBeforeRelationStatusLabel(resolveRelationStatusLabel(log.getBeforeRelationStatus()));
        vo.setAfterRelationStatus(log.getAfterRelationStatus());
        vo.setAfterRelationStatusLabel(resolveRelationStatusLabel(log.getAfterRelationStatus()));
        vo.setBeforeExpireAt(log.getBeforeExpireAt());
        vo.setAfterExpireAt(log.getAfterExpireAt());
        vo.setReason(formatLogReason(log));
        vo.setChangeSummary(buildChangeSummary(log));
        vo.setOperatorType(log.getOperatorType());
        vo.setOperatorTypeLabel(resolveOperatorTypeLabel(log.getOperatorType()));
        vo.setCreateTime(log.getCreateTime());
        return vo;
    }

    /**
     * 根据分销员ID解析分销员手机号。
     *
     * @param distributionId 分销员ID
     * @return 手机号，无法解析时返回 null
     */
    private String resolveDistributionMobile(String distributionId) {
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
     *
     * @param distributionId 分销员ID
     * @return 昵称，无则返回 ID 或 "-"
     */
    private String resolveDistributionNickname(String distributionId) {
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
     *
     * @param log 变更日志
     * @return 变更类型标签
     */
    private String resolveChangeTypeLabel(DistributionCustomerRelationLog log) {
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
     *
     * @param log 变更日志
     * @return 摘要文本
     */
    private String buildChangeSummary(DistributionCustomerRelationLog log) {
        if (log == null || log.getChangeType() == null) {
            return "-";
        }
        String beforeName = resolveDistributionNickname(log.getBeforeDistributionId());
        String afterName = resolveDistributionNickname(log.getAfterDistributionId());
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
     *
     * @param operatorType 操作人类型
     * @return 展示文案
     */
    private String resolveOperatorTypeLabel(String operatorType) {
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
     *
     * @param relationStatus 关系状态
     * @return 展示文案
     */
    private String resolveRelationStatusLabel(String relationStatus) {
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
     *
     * @param log 变更日志
     * @return 格式化后的原因
     */
    private String formatLogReason(DistributionCustomerRelationLog log) {
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
     * 判断客户关系是否有效（非失效、非解绑）。
     *
     * @param relation 客户关系
     * @return 是否有效
     */
    private boolean isRelationEffective(DistributionCustomerRelation relation) {
        if (relation == null || relation.getRelationStatus() == null) {
            return false;
        }
        String status = relation.getRelationStatus();
        return !DistributionRelationStatusEnum.EXPIRED.name().equals(status)
                && !DistributionRelationStatusEnum.UNBOUND.name().equals(status);
    }

    /**
     * 为无效关系填充失效时间与失效原因。
     *
     * @param vo 列表 VO
     * @param relation 客户关系
     */
    private void fillInvalidInfo(DistributionCustomerRelationVO vo, DistributionCustomerRelation relation) {
        // 优先从失效/解绑日志取原因，无日志则按状态兜底
        DistributionCustomerRelationLog log = relationLogMapper.selectOne(
                new LambdaQueryWrapper<DistributionCustomerRelationLog>()
                        .eq(DistributionCustomerRelationLog::getRelationId, relation.getId())
                        .in(DistributionCustomerRelationLog::getChangeType, "EXPIRE", "ADMIN_UNBIND")
                        .orderByDesc(DistributionCustomerRelationLog::getCreateTime)
                        .last("limit 1"));
        if (log != null) {
            vo.setInvalidTime(log.getCreateTime());
            vo.setInvalidReason(resolveInvalidReason(log));
            return;
        }
        vo.setInvalidTime(relation.getUpdateTime());
        vo.setInvalidReason(resolveInvalidReasonByStatus(relation.getRelationStatus()));
    }

    /**
     * 根据变更日志解析失效原因。
     *
     * @param log 变更日志
     * @return 失效原因文案
     */
    private String resolveInvalidReason(DistributionCustomerRelationLog log) {
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
     *
     * @param relationStatus 关系状态
     * @return 失效原因文案
     */
    private String resolveInvalidReasonByStatus(String relationStatus) {
        if (DistributionRelationStatusEnum.EXPIRED.name().equals(relationStatus)) {
            return "客户关系失效";
        }
        if (DistributionRelationStatusEnum.UNBOUND.name().equals(relationStatus)) {
            return "已解绑";
        }
        return "-";
    }

    /**
     * 返回默认客户绑定配置。
     *
     * @return 默认配置
     */
    private DistributionCustomerSetting defaultCustomerSetting() {
        DistributionCustomerSetting setting = new DistributionCustomerSetting();
        setting.setBindMode(DistributionBindModeEnum.SHORT_TERM.name());
        setting.setPermanentBindMode(DistributionPermanentBindModeEnum.PAID_WITHIN_DAYS.name());
        setting.setPermanentPaidDays(1);
        setting.setShortTermBindDays(7);
        return setting;
    }

    /**
     * 判断客户关系是否处于保护期内（有效且未过期）。
     *
     * @param relation 客户关系
     * @param now 当前时间
     * @return 是否受保护
     */
    private boolean isProtectedRelation(DistributionCustomerRelation relation, Date now) {
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
