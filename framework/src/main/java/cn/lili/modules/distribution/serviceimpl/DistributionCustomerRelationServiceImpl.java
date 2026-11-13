package cn.lili.modules.distribution.serviceimpl;

import cn.lili.modules.distribution.support.DistributionBuyerCustomerSupport;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelationLog;
import cn.lili.modules.distribution.entity.dto.DistributionBuyerCustomerQueryDTO;
import cn.lili.modules.distribution.entity.dto.DistributionCustomerRelationSearchParams;
import cn.lili.modules.distribution.entity.enums.DistributionRelationStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerPageVO;
import cn.lili.modules.distribution.entity.vos.DistributionCustomerRelationDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionCustomerRelationLogVO;
import cn.lili.modules.distribution.entity.vos.DistributionCustomerRelationVO;
import cn.lili.modules.distribution.mapper.DistributionCustomerRelationLogMapper;
import cn.lili.modules.distribution.mapper.DistributionCustomerRelationMapper;
import cn.lili.modules.distribution.mapper.DistributionMapper;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.support.DistributionCustomerRelationBindSupport;
import cn.lili.modules.distribution.support.DistributionCustomerRelationLogSupport;
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

import java.util.Date;
import java.util.List;
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
            return DistributionCustomerRelationBindSupport.defaultCustomerSetting();
        }
        return JSONUtil.toBean(setting.getSettingValue(), DistributionCustomerSetting.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void bindOnScan(String distributionId, String memberId, String source) {
        DistributionCustomerRelationBindSupport.bindOnScan(this, distributionMemberSupport, relationLogMapper, distributionMapper, cache, distributionId, memberId, source);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void bindPermanentOnPaid(String memberId, String orderSn) {
        DistributionCustomerRelationBindSupport.bindPermanentOnPaid(this, relationLogMapper, distributionMapper, memberId, orderSn);
    }

    @Override
    public DistributionCustomerRelation getActiveRelation(String memberId) {
        return DistributionCustomerRelationBindSupport.getActiveRelation(this, memberId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void adminRebind(String memberId, String distributionId, String reason, String operatorId) {
        DistributionCustomerRelationBindSupport.adminRebind(this, relationLogMapper, distributionMapper, cache, memberId, distributionId, reason, operatorId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void adminUnbind(String memberId, String reason, String operatorId) {
        DistributionCustomerRelationBindSupport.adminUnbind(this, relationLogMapper, distributionMapper, cache, memberId, reason, operatorId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int expirePendingRelations() {
        return DistributionCustomerRelationBindSupport.expirePendingRelations(this, relationLogMapper, distributionMapper, cache);
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
        return DistributionCustomerRelationLogSupport.listRelationLogs(relationLogMapper, memberService, distributionMemberSupport, relationId);
    }

    @Override
    public DistributionBuyerCustomerPageVO pageBuyerCustomers(String distributionId, DistributionBuyerCustomerQueryDTO query) {
        return DistributionBuyerCustomerSupport.pageBuyerCustomers(baseMapper, distributionId, query);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateBuyerCustomerStarred(String distributionId, String relationId, Boolean starred) {
        DistributionBuyerCustomerSupport.updateBuyerCustomerStarred(this, distributionId, relationId, starred);
    }

    @Override
    public DistributionBuyerCustomerDetailVO getBuyerCustomerDetail(String distributionId, String relationId) {
        if (StringUtils.isBlank(distributionId) || StringUtils.isBlank(relationId)) {
            throw new ServiceException(ResultCode.ERROR);
        }
        DistributionCustomerRelation relation = getById(relationId);
        return DistributionBuyerCustomerSupport.getBuyerCustomerDetail(baseMapper, memberService, relation, distributionId);
    }

    private Page<DistributionCustomerRelationVO> emptyRelationPage(PageVO page) {
        Page<DistributionCustomerRelationVO> result = new Page<>(page.getPageNumber(), page.getPageSize(), 0);
        result.setRecords(List.of());
        return result;
    }

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
            fillInvalidInfo(vo, relation);
        }
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

    private DistributionCustomerRelationDetailVO toDetailVO(DistributionCustomerRelation relation) {
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

    private boolean isRelationEffective(DistributionCustomerRelation relation) {
        if (relation == null || relation.getRelationStatus() == null) {
            return false;
        }
        String status = relation.getRelationStatus();
        return !DistributionRelationStatusEnum.EXPIRED.name().equals(status)
                && !DistributionRelationStatusEnum.UNBOUND.name().equals(status);
    }

    private void fillInvalidInfo(DistributionCustomerRelationVO vo, DistributionCustomerRelation relation) {
        DistributionCustomerRelationLog log = relationLogMapper.selectOne(
                new LambdaQueryWrapper<DistributionCustomerRelationLog>()
                        .eq(DistributionCustomerRelationLog::getRelationId, relation.getId())
                        .in(DistributionCustomerRelationLog::getChangeType, "EXPIRE", "ADMIN_UNBIND")
                        .orderByDesc(DistributionCustomerRelationLog::getCreateTime)
                        .last("limit 1"));
        if (log != null) {
            vo.setInvalidTime(log.getCreateTime());
            vo.setInvalidReason(DistributionCustomerRelationLogSupport.resolveInvalidReason(log));
            return;
        }
        vo.setInvalidTime(relation.getUpdateTime());
        vo.setInvalidReason(DistributionCustomerRelationLogSupport.resolveInvalidReasonByStatus(relation.getRelationStatus()));
    }
}
