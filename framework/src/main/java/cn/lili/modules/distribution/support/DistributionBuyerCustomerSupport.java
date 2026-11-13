package cn.lili.modules.distribution.support;

import cn.lili.modules.distribution.serviceimpl.DistributionCustomerRelationServiceImpl;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.entity.dto.DistributionBuyerCustomerQueryDTO;
import cn.lili.modules.distribution.entity.enums.DistributionRelationStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerPageVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerTradeStatsVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerVO;
import cn.lili.modules.distribution.mapper.DistributionCustomerRelationMapper;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.List;

/**
 * 分销员买家客户列表、星标更新与客户成交详情支持类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
public class DistributionBuyerCustomerSupport {

    /**
     * 分页查询分销员下的买家客户列表。
     *
     * @param baseMapper 关系持久层映射
     * @param distributionId 分销员ID
     * @param query 查询参数
     * @return 买家客户分页 VO
     */
    public static DistributionBuyerCustomerPageVO pageBuyerCustomers(DistributionCustomerRelationMapper baseMapper,
                                                                     String distributionId,
                                                                     DistributionBuyerCustomerQueryDTO query) {
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

    /**
     * 更新客户星标状态。
     *
     * @param service 关系服务
     * @param distributionId 分销员ID
     * @param relationId 关系ID
     * @param starred 是否星标
     */
    public static void updateBuyerCustomerStarred(DistributionCustomerRelationServiceImpl service,
                                                 String distributionId,
                                                 String relationId,
                                                 Boolean starred) {
        if (StringUtils.isBlank(distributionId) || StringUtils.isBlank(relationId) || starred == null) {
            throw new ServiceException(ResultCode.ERROR);
        }
        DistributionCustomerRelation relation = service.getById(relationId);
        if (relation == null || !distributionId.equals(relation.getDistributionId())) {
            throw new ServiceException(ResultCode.ERROR);
        }
        relation.setStarred(Boolean.TRUE.equals(starred));
        service.updateById(relation);
    }

    /**
     * 查询买家客户推广详情与累计成交统计。
     *
     * @param baseMapper 关系持久层映射
     * @param memberService 会员服务
     * @param relation 客户关系实体
     * @param distributionId 分销员ID
     * @return 客户详情 VO
     */
    public static DistributionBuyerCustomerDetailVO getBuyerCustomerDetail(DistributionCustomerRelationMapper baseMapper,
                                                                           MemberService memberService,
                                                                           DistributionCustomerRelation relation,
                                                                           String distributionId) {
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
     * 补全买家客户列表展示字段（有效性、状态文案、星标默认值）。
     */
    public static void fillBuyerCustomerDisplay(DistributionBuyerCustomerVO record) {
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
            record.setRelationStatusText(DistributionCustomerRelationLogSupport.resolveRelationStatusLabel(status));
        }
        if (record.getStarred() == null) {
            record.setStarred(Boolean.FALSE);
        }
    }
}
