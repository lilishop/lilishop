package cn.lili.modules.promotion.support;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.order.mapper.OrderGiftCardRecordMapper;
import cn.lili.modules.promotion.entity.dos.GiftCardCashActivity;
import cn.lili.modules.promotion.entity.dos.GiftCardCashCredential;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashCredentialSearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashMemberCardSearchParams;
import cn.lili.modules.promotion.entity.enums.GiftCardCashCredentialStatusEnum;
import cn.lili.modules.promotion.entity.enums.GiftCardCashValidityTypeEnum;
import cn.lili.modules.promotion.entity.vos.GiftCardCashCredentialVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashMemberCardVO;
import cn.lili.modules.promotion.mapper.GiftCardCashActivityMapper;
import cn.lili.modules.promotion.mapper.GiftCardCashCredentialMapper;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 现金/提货礼品卡卡密凭证查询、买家端会员卡展示、绑定、激活与过期失效支持类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
public class GiftCardCashCredentialSupport {

    /**
     * 分页查询卡密凭证。
     * 支持按会员手机号筛选，内部会先解析会员ID再构造in查询。
     *
     * @param credentialMapper 卡密凭证持久层映射
     * @param memberService 会员服务
     * @param params 筛选条件
     * @param page 分页参数
     * @return 卡密凭证分页展示对象
     */
    public static IPage<GiftCardCashCredentialVO> pageCredential(GiftCardCashCredentialMapper credentialMapper,
                                                                 MemberService memberService,
                                                                 GiftCardCashCredentialSearchParams params,
                                                                 PageVO page) {
        Page<GiftCardCashCredential> mpPage = PageUtil.initPage(page);
        LambdaQueryWrapper<GiftCardCashCredential> w = new LambdaQueryWrapper<>();
        w.eq(GiftCardCashCredential::getDeleteFlag, false);
        if (params != null) {
            if (CharSequenceUtil.isNotBlank(params.getActivityId())) {
                w.eq(GiftCardCashCredential::getActivityId, params.getActivityId().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getCreateBatchId())) {
                w.eq(GiftCardCashCredential::getCreateBatchId, params.getCreateBatchId());
            }
            if (CharSequenceUtil.isNotBlank(params.getIssueBatchId())) {
                w.eq(GiftCardCashCredential::getIssueBatchId, params.getIssueBatchId());
            }
            if (CharSequenceUtil.isNotBlank(params.getCardNo())) {
                w.like(GiftCardCashCredential::getCardNo, params.getCardNo().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getStatus())) {
                w.eq(GiftCardCashCredential::getStatus, params.getStatus().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getMemberMobile())) {
                List<Member> members = memberService.list(new LambdaQueryWrapper<Member>()
                        .eq(Member::getMobile, params.getMemberMobile().trim())
                        .eq(Member::getDeleteFlag, false));
                if (CollUtil.isEmpty(members)) {
                    Page<GiftCardCashCredentialVO> empty = new Page<>(mpPage.getCurrent(), mpPage.getSize(), 0);
                    empty.setRecords(Collections.emptyList());
                    return empty;
                }
                List<String> mids = members.stream().map(Member::getId).collect(Collectors.toList());
                w.in(GiftCardCashCredential::getMemberId, mids);
            }
        }
        w.orderByDesc(GiftCardCashCredential::getCreateTime);
        IPage<GiftCardCashCredential> raw = credentialMapper.selectPage(mpPage, w);
        return raw.convert(c -> toCredentialVo(memberService, c));
    }

    /**
     * 定时或被动触发扫描过期失效卡密凭证。
     *
     * @param credentialMapper 卡密持久层映射
     * @param activityMapper 活动持久层映射
     * @return 实际标记过期的卡密记录总数
     */
    public static int expireOverdueCredentials(GiftCardCashCredentialMapper credentialMapper,
                                               GiftCardCashActivityMapper activityMapper) {
        Date now = new Date();
        int total = 0;
        total += credentialMapper.update(null, new LambdaUpdateWrapper<GiftCardCashCredential>()
                .set(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.EXPIRED.name())
                .eq(GiftCardCashCredential::getDeleteFlag, false)
                .in(GiftCardCashCredential::getStatus,
                        GiftCardCashCredentialStatusEnum.UNISSUED.name(),
                        GiftCardCashCredentialStatusEnum.BOUND.name())
                .isNotNull(GiftCardCashCredential::getExpireTime)
                .le(GiftCardCashCredential::getExpireTime, now));
        List<GiftCardCashActivity> expiredActivities = activityMapper.selectList(new LambdaQueryWrapper<GiftCardCashActivity>()
                .eq(GiftCardCashActivity::getDeleteFlag, false)
                .eq(GiftCardCashActivity::getValidityType, GiftCardCashValidityTypeEnum.FIXED_UNTIL.name())
                .isNotNull(GiftCardCashActivity::getValidEndTime)
                .le(GiftCardCashActivity::getValidEndTime, now));
        if (CollUtil.isNotEmpty(expiredActivities)) {
            List<String> activityIds = expiredActivities.stream()
                    .map(GiftCardCashActivity::getId)
                    .collect(Collectors.toList());
            total += credentialMapper.update(null, new LambdaUpdateWrapper<GiftCardCashCredential>()
                    .set(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.EXPIRED.name())
                    .eq(GiftCardCashCredential::getDeleteFlag, false)
                    .eq(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.UNISSUED.name())
                    .in(GiftCardCashCredential::getActivityId, activityIds));
            total += credentialMapper.update(null, new LambdaUpdateWrapper<GiftCardCashCredential>()
                    .set(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.EXPIRED.name())
                    .eq(GiftCardCashCredential::getDeleteFlag, false)
                    .eq(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.BOUND.name())
                    .isNull(GiftCardCashCredential::getActivateTime)
                    .isNull(GiftCardCashCredential::getExpireTime)
                    .in(GiftCardCashCredential::getActivityId, activityIds));
        }
        if (total > 0) {
            log.info("礼品卡过期扫描完成，更新凭证数量：{}", total);
        }
        return total;
    }

    /**
     * 买家端分页查询会员礼品卡。
     * <p>
     * 状态筛选规则：
     * <ul>
     *   <li>PENDING_ACTIVATION：绑定会员=当前会员，且激活会员为空；</li>
     *   <li>AVAILABLE：当前会员已激活且未过期；现金卡余额大于0，提货卡未兑换；</li>
     *   <li>UNAVAILABLE：当前会员已激活，现金卡余额耗尽、提货卡已兑换或卡已过期。</li>
     * </ul>
     * 未传状态时，按绑定会员（memberId）查询当前会员的全部礼品卡。
     *
     * @param credentialMapper 卡密凭证映射
     * @param activityMapper 活动持久层映射
     * @param memberId 会员ID
     * @param params 筛选条件
     * @param page 分页参数
     * @return 会员礼品卡展示对象分页结果
     */
    public static IPage<GiftCardCashMemberCardVO> pageMemberCards(GiftCardCashCredentialMapper credentialMapper,
                                                                 GiftCardCashActivityMapper activityMapper,
                                                                 String memberId,
                                                                 GiftCardCashMemberCardSearchParams params,
                                                                 PageVO page) {
        Page<GiftCardCashCredential> mpPage = PageUtil.initPage(page);
        LambdaQueryWrapper<GiftCardCashCredential> w = new LambdaQueryWrapper<>();
        w.eq(GiftCardCashCredential::getDeleteFlag, false);
        if (params != null && CharSequenceUtil.isNotBlank(params.getMemberCardStatus())) {
            Date now = new Date();
            if ("PENDING_ACTIVATION".equals(params.getMemberCardStatus())) {
                w.eq(GiftCardCashCredential::getMemberId, memberId)
                        .isNull(GiftCardCashCredential::getActivateMemberId);
            } else if ("AVAILABLE".equals(params.getMemberCardStatus())) {
                w.eq(GiftCardCashCredential::getActivateMemberId, memberId)
                        .and(q -> q.eq(GiftCardCashCredential::getCardType, "PICKUP").isNull(GiftCardCashCredential::getPickupTradeSn)
                                .or(x -> x.eq(GiftCardCashCredential::getCardType, "CASH").gt(GiftCardCashCredential::getBalance, BigDecimal.ZERO)))
                        .eq(GiftCardCashCredential::getStatus, "BOUND")
                        .and(q -> q.isNull(GiftCardCashCredential::getExpireTime)
                                .or()
                                .gt(GiftCardCashCredential::getExpireTime, now));
            } else {
                w.eq(GiftCardCashCredential::getActivateMemberId, memberId)
                        .and(q -> q.eq(GiftCardCashCredential::getStatus, "REDEEMED")
                                .or(x -> x.eq(GiftCardCashCredential::getCardType, "CASH").le(GiftCardCashCredential::getBalance, BigDecimal.ZERO))
                                .or(x -> x.isNotNull(GiftCardCashCredential::getExpireTime)
                                        .le(GiftCardCashCredential::getExpireTime, now)));
            }
        } else {
            w.eq(GiftCardCashCredential::getMemberId, memberId);
        }
        w.orderByDesc(GiftCardCashCredential::getCreateTime);
        IPage<GiftCardCashCredential> raw = credentialMapper.selectPage(mpPage, w);
        return raw.convert(c -> toMemberCardVo(activityMapper, c));
    }

    /**
     * 买家通过卡号和卡密兑换绑定礼品卡到本人账户。
     *
     * @param credentialMapper 卡密凭证映射
     * @param activityMapper 活动映射
     * @param memberId 会员ID
     * @param cardNo 卡号
     * @param cardSecret 卡密
     */
    public static void bindMemberCard(GiftCardCashCredentialMapper credentialMapper,
                                      GiftCardCashActivityMapper activityMapper,
                                      String memberId,
                                      String cardNo,
                                      String cardSecret) {
        String safeCardNo = CharSequenceUtil.trim(cardNo);
        String safeCardSecret = CharSequenceUtil.trim(cardSecret);
        if (CharSequenceUtil.isBlank(safeCardNo) || CharSequenceUtil.isBlank(safeCardSecret)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "卡号和兑换码不能为空");
        }
        GiftCardCashCredential credential = credentialMapper.selectOne(new LambdaQueryWrapper<GiftCardCashCredential>()
                .eq(GiftCardCashCredential::getCardNo, safeCardNo)
                .eq(GiftCardCashCredential::getDeleteFlag, false)
                .last("LIMIT 1"));
        if (credential == null || !safeCardSecret.equals(credential.getCardSecret())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "卡号或兑换码错误");
        }
        if (!GiftCardCashCredentialStatusEnum.UNISSUED.name().equals(credential.getStatus())) {
            if (GiftCardCashCredentialStatusEnum.BOUND.name().equals(credential.getStatus())
                    && Objects.equals(memberId, credential.getMemberId())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "该礼品卡已绑定当前账号");
            }
            throw new ServiceException(ResultCode.PARAMS_ERROR, "该礼品卡当前状态不可绑定");
        }
        GiftCardCashActivity activity = getActivityOrThrow(activityMapper, credential.getActivityId());
        Date now = new Date();
        Date fixedEndTime = activity.getValidEndTime();
        if (GiftCardCashValidityTypeEnum.FIXED_UNTIL.name().equals(activity.getValidityType())
                && fixedEndTime != null && !fixedEndTime.after(now)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡已过期，无法绑定");
        }
        int updated = credentialMapper.update(null, new LambdaUpdateWrapper<GiftCardCashCredential>()
                .set(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.BOUND.name())
                .set(GiftCardCashCredential::getMemberId, memberId)
                .set(GiftCardCashCredential::getBindTime, now)
                .set(GiftCardCashCredential::getActivateMemberId, null)
                .set(GiftCardCashCredential::getActivateTime, null)
                .set(GiftCardCashCredential::getExpireTime, null)
                .set(GiftCardCashCredential::getBalance, credential.getFaceValue())
                .eq(GiftCardCashCredential::getId, credential.getId())
                .eq(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.UNISSUED.name())
                .eq(GiftCardCashCredential::getDeleteFlag, false));
        if (updated != 1) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡绑定失败，请稍后重试");
        }
    }

    /**
     * 激活已绑定的礼品卡。
     *
     * @param credentialMapper 卡密凭证映射
     * @param activityMapper 活动持久层映射
     * @param orderGiftCardRecordMapper 交易流水映射
     * @param memberId 会员ID
     * @param memberCardId 凭证ID
     */
    public static void activateMemberCard(GiftCardCashCredentialMapper credentialMapper,
                                          GiftCardCashActivityMapper activityMapper,
                                          OrderGiftCardRecordMapper orderGiftCardRecordMapper,
                                          String memberId,
                                          String memberCardId) {
        if (CharSequenceUtil.isBlank(memberCardId)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡ID不能为空");
        }
        GiftCardCashCredential credential = credentialMapper.selectById(memberCardId);
        if (credential == null || Boolean.TRUE.equals(credential.getDeleteFlag())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡不存在");
        }
        if (!Objects.equals(memberId, credential.getMemberId())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "无权激活该礼品卡");
        }
        if (!GiftCardCashCredentialStatusEnum.BOUND.name().equals(credential.getStatus())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "当前礼品卡状态不可激活");
        }
        if (credential.getActivateTime() != null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡已激活，请勿重复操作");
        }
        GiftCardCashActivity activity = getActivityOrThrow(activityMapper, credential.getActivityId());
        Date now = new Date();
        Date expireTime = calculateExpireTime(activity, now);
        if (expireTime != null && !expireTime.after(now)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡已过期，无法激活");
        }
        int updated = credentialMapper.update(null, new LambdaUpdateWrapper<GiftCardCashCredential>()
                .set(GiftCardCashCredential::getActivateMemberId, memberId)
                .set(GiftCardCashCredential::getActivateTime, now)
                .set(GiftCardCashCredential::getExpireTime, expireTime)
                .set(GiftCardCashCredential::getBalance, credential.getFaceValue())
                .eq(GiftCardCashCredential::getId, memberCardId)
                .eq(GiftCardCashCredential::getMemberId, memberId)
                .eq(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.BOUND.name())
                .isNull(GiftCardCashCredential::getActivateTime)
                .eq(GiftCardCashCredential::getDeleteFlag, false));
        if (updated != 1) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡激活失败，请稍后重试");
        }
        GiftCardCashCredential refreshed = credentialMapper.selectById(memberCardId);
        if (refreshed != null && !"PICKUP".equals(refreshed.getCardType())) {
            GiftCardCashTradeSupport.saveGiftCardUsageRecord(orderGiftCardRecordMapper, null, null, memberId, refreshed, refreshed.getFaceValue(),
                    "INCREASE", "CARD_ACTIVATE", memberCardId, refreshed.getBalance());
        }
    }

    /**
     * 查询买家当前可用现金礼品卡列表（排除提货卡）。
     *
     * @param credentialMapper 卡密映射
     * @param activityMapper 活动映射
     * @param memberId 会员ID
     * @return 可用礼品卡展示对象列表
     */
    public static List<GiftCardCashMemberCardVO> listAvailableMemberCards(GiftCardCashCredentialMapper credentialMapper,
                                                                         GiftCardCashActivityMapper activityMapper,
                                                                         String memberId) {
        Date now = new Date();
        List<GiftCardCashCredential> list = credentialMapper.selectList(new LambdaQueryWrapper<GiftCardCashCredential>()
                .eq(GiftCardCashCredential::getDeleteFlag, false)
                .eq(GiftCardCashCredential::getActivateMemberId, memberId)
                .eq(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.BOUND.name())
                .gt(GiftCardCashCredential::getBalance, BigDecimal.ZERO)
                .and(q -> q.isNull(GiftCardCashCredential::getExpireTime)
                        .or()
                        .gt(GiftCardCashCredential::getExpireTime, now))
                .orderByAsc(GiftCardCashCredential::getExpireTime)
                .orderByAsc(GiftCardCashCredential::getCreateTime));
        list.removeIf(c -> "PICKUP".equals(c.getCardType()));
        return list.stream().map(c -> toMemberCardVo(activityMapper, c)).collect(Collectors.toList());
    }

    /** 将卡密实体转换为展示VO，并补齐会员手机号/昵称。 */
    private static GiftCardCashCredentialVO toCredentialVo(MemberService memberService, GiftCardCashCredential c) {
        GiftCardCashCredentialVO vo = new GiftCardCashCredentialVO();
        BeanUtils.copyProperties(c, vo);
        if (CharSequenceUtil.isNotBlank(c.getMemberId())) {
            Member m = memberService.getById(c.getMemberId());
            if (m != null) {
                vo.setMemberMobile(m.getMobile());
                vo.setMemberNickName(m.getNickName());
            }
        }
        return vo;
    }

    /** 会员礼品卡VO转换。 */
    public static GiftCardCashMemberCardVO toMemberCardVo(GiftCardCashActivityMapper activityMapper, GiftCardCashCredential credential) {
        GiftCardCashMemberCardVO vo = new GiftCardCashMemberCardVO();
        BeanUtils.copyProperties(credential, vo);
        vo.setCardSecret(null);
        GiftCardCashActivity activity = activityMapper.selectById(credential.getActivityId());
        if (activity != null && !Boolean.TRUE.equals(activity.getDeleteFlag())) {
            vo.setGiftCardName(activity.getGiftCardName());
            vo.setPickupMode(activity.getPickupMode());
            vo.setPickupCount(activity.getPickupCount());
        }
        vo.setMemberCardStatus(resolveMemberCardStatus(credential, new Date()));
        return vo;
    }

    /** 根据凭证信息计算买家端展示状态。 */
    public static String resolveMemberCardStatus(GiftCardCashCredential credential, Date now) {
        if (GiftCardCashCredentialStatusEnum.EXPIRED.name().equals(credential.getStatus())) {
            return "UNAVAILABLE";
        }
        Date activatedAt = credential.getActivateTime();
        if (activatedAt == null && credential.getBindTime() != null && credential.getExpireTime() != null) {
            // 兼容旧数据：历史流程用 bindTime 表示激活时间
            activatedAt = credential.getBindTime();
        }
        if (activatedAt == null) {
            return "PENDING_ACTIVATION";
        }
        boolean isBound = GiftCardCashCredentialStatusEnum.BOUND.name().equals(credential.getStatus());
        boolean notExpired = credential.getExpireTime() == null || credential.getExpireTime().after(now);
        boolean hasBalance = "PICKUP".equals(credential.getCardType()) ? credential.getPickupTradeSn() == null
                : credential.getBalance() != null && credential.getBalance().compareTo(BigDecimal.ZERO) > 0;
        if (isBound && notExpired && hasBalance) {
            return "AVAILABLE";
        }
        return "UNAVAILABLE";
    }

    /**
     * 绑定时按活动有效期规则计算卡到期时间：
     * LONG_TERM: null（长期有效）
     * FIXED_UNTIL: 活动固定到期时间
     * AFTER_ACTIVATE_MONTHS: 绑定时间 + N个月
     */
    public static Date calculateExpireTime(GiftCardCashActivity activity, Date bindTime) {
        if (activity == null || CharSequenceUtil.isBlank(activity.getValidityType())) {
            return null;
        }
        if (GiftCardCashValidityTypeEnum.FIXED_UNTIL.name().equals(activity.getValidityType())) {
            return activity.getValidEndTime();
        }
        if (GiftCardCashValidityTypeEnum.AFTER_ACTIVATE_MONTHS.name().equals(activity.getValidityType())
                && activity.getValidMonthsAfterActivate() != null && bindTime != null) {
            Calendar calendar = Calendar.getInstance();
            calendar.setTime(bindTime);
            calendar.add(Calendar.MONTH, activity.getValidMonthsAfterActivate());
            return calendar.getTime();
        }
        return null;
    }

    private static GiftCardCashActivity getActivityOrThrow(GiftCardCashActivityMapper activityMapper, String id) {
        GiftCardCashActivity a = activityMapper.selectById(id);
        if (a == null || Boolean.TRUE.equals(a.getDeleteFlag())) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_ACTIVITY_NOT_FOUND);
        }
        return a;
    }
}
