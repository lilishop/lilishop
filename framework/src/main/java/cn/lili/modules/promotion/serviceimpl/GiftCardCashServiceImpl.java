package cn.lili.modules.promotion.serviceimpl;

import cn.lili.modules.promotion.support.GiftCardCashBatchSupport;
import cn.lili.modules.promotion.support.GiftCardCashCredentialSupport;
import cn.lili.modules.promotion.support.GiftCardCashTradeSupport;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.mapper.OrderGiftCardRecordMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.permission.service.AdminUserService;
import cn.lili.modules.promotion.entity.dos.GiftCardCashActivity;
import cn.lili.modules.promotion.entity.dos.GiftCardCashCreateBatch;
import cn.lili.modules.promotion.entity.dto.GiftCardCashActivityDTO;
import cn.lili.modules.promotion.entity.dto.GiftCardCashBatchCreateDTO;
import cn.lili.modules.promotion.entity.dto.GiftCardCashDeductPreviewDTO;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashActivitySearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashCreateBatchSearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashCredentialSearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashIssueBatchSearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashMemberCardSearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardUsageRecordSearchParams;
import cn.lili.modules.promotion.entity.enums.GiftCardCashCredentialStatusEnum;
import cn.lili.modules.promotion.entity.enums.GiftCardCashValidityTypeEnum;
import cn.lili.modules.promotion.entity.enums.GiftCardTypeEnum;
import cn.lili.modules.promotion.entity.vos.GiftCardCashActivityVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashCreateBatchVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashCredentialVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashDeductPreviewVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashIssueBatchVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashMemberCardVO;
import cn.lili.modules.promotion.entity.vos.GiftCardUsageRecordVO;
import cn.lili.modules.promotion.mapper.GiftCardCashActivityMapper;
import cn.lili.modules.promotion.mapper.GiftCardCashCreateBatchMapper;
import cn.lili.modules.promotion.mapper.GiftCardCashCredentialMapper;
import cn.lili.modules.promotion.mapper.GiftCardCashIssueBatchMapper;
import cn.lili.modules.promotion.service.GiftCardCashService;
import cn.lili.modules.promotion.service.PickupCardRuleService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * 现金礼品卡服务实现类（活动、制卡、发卡、绑定、抵扣、退款等核心业务）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Slf4j
@Service
public class GiftCardCashServiceImpl implements GiftCardCashService {

    @Autowired
    private GiftCardCashActivityMapper activityMapper;

    @Autowired
    private GiftCardCashCreateBatchMapper createBatchMapper;

    @Autowired
    private GiftCardCashIssueBatchMapper issueBatchMapper;

    @Autowired
    private GiftCardCashCredentialMapper credentialMapper;

    @Autowired
    private MemberService memberService;

    @Autowired
    private AdminUserService adminUserService;

    @Autowired
    private OrderGiftCardRecordMapper orderGiftCardRecordMapper;

    @Autowired
    private OrderItemService orderItemService;

    @Autowired
    private PickupCardRuleService pickupCardRuleService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String saveActivity(GiftCardCashActivityDTO dto) {
        normalizeActivityDto(dto);
        String id = SnowFlake.getIdStr();
        GiftCardCashActivity entity = new GiftCardCashActivity();
        entity.setId(id);
        copyDtoToActivity(entity, dto);
        activityMapper.insert(entity);
        return id;
    }

    /**
     * 更新礼品卡活动。
     * 已经产卡后，只允许改活动名称和总库存，且库存不能小于已产数量。
     * 活动创建后禁止切换现金卡与提货卡类型；未传类型时沿用已保存类型，避免旧调用方误切换。
     *
     * @param id 活动 ID
     * @param dto 经服务端校验后保存的活动参数
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateActivity(String id, GiftCardCashActivityDTO dto) {
        GiftCardCashActivity current = lockActivity(id);
        String cardType = CharSequenceUtil.isBlank(current.getCardType())
                ? GiftCardTypeEnum.CASH.name() : current.getCardType();
        if (CharSequenceUtil.isNotBlank(dto.getCardType()) && !cardType.equals(dto.getCardType().trim())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡创建后不允许切换现金卡与提货卡类型");
        }
        dto.setCardType(cardType);
        long produced = countCredentialsByActivity(id);
        if (produced > 0 && "PICKUP".equals(current.getCardType()) && "PICKUP".equals(dto.getCardType())) {
            pickupCardRuleService.validateSelection(dto.getPickupMode(), dto.getPickupCount(), dto.getPickupSkuIds(), dto.getPickupSkuIds(), false);
            dto.setFaceValue(BigDecimal.ZERO.setScale(2));
            if ("ONE".equals(dto.getPickupMode())) {
                dto.setPickupCount(1);
            }
            if ("ALL".equals(dto.getPickupMode())) {
                dto.setPickupCount(dto.getPickupSkuIds().size());
            }
        } else {
            normalizeActivityDto(dto);
        }
        if (produced > 0) {
            if (dto.getStockTotal() < produced) {
                throw new ServiceException(ResultCode.GIFT_CARD_CASH_ACTIVITY_STOCK_LT_PRODUCED);
            }
            if (!activityCoreEquals(current, dto)) {
                throw new ServiceException(ResultCode.GIFT_CARD_CASH_ACTIVITY_EDIT_FORBIDDEN);
            }
            current.setGiftCardName(dto.getGiftCardName().trim());
            current.setStockTotal(dto.getStockTotal());
            activityMapper.updateById(current);
            return;
        }
        copyDtoToActivity(current, dto);
        activityMapper.updateById(current);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteActivity(String id) {
        lockActivity(id);
        long produced = countCredentialsByActivity(id);
        long batches = createBatchMapper.selectCount(new LambdaQueryWrapper<GiftCardCashCreateBatch>()
                .eq(GiftCardCashCreateBatch::getActivityId, id)
                .eq(GiftCardCashCreateBatch::getDeleteFlag, false));
        if (produced > 0 || batches > 0) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_ACTIVITY_DELETE_FORBIDDEN);
        }
        activityMapper.update(null, new LambdaUpdateWrapper<GiftCardCashActivity>()
                .set(GiftCardCashActivity::getDeleteFlag, true)
                .eq(GiftCardCashActivity::getId, id)
                .eq(GiftCardCashActivity::getDeleteFlag, false));
    }

    @Override
    public GiftCardCashActivityVO getActivity(String id) {
        GiftCardCashActivityVO vo = toActivityVo(getActivityOrThrow(id));
        if ("PICKUP".equals(vo.getCardType())) {
            vo.setPickupGoods(pickupCardRuleService.goods(vo.getPickupSkuIds()));
        }
        return vo;
    }

    @Override
    public IPage<GiftCardCashActivityVO> pageActivity(GiftCardCashActivitySearchParams params, PageVO page) {
        Page<GiftCardCashActivity> mpPage = PageUtil.initPage(page);
        LambdaQueryWrapper<GiftCardCashActivity> w = new LambdaQueryWrapper<>();
        w.eq(GiftCardCashActivity::getDeleteFlag, false);
        if (params != null) {
            if (CharSequenceUtil.isNotBlank(params.getGiftCardName())) {
                w.like(GiftCardCashActivity::getGiftCardName, params.getGiftCardName().trim());
            }
            if (params.getFaceValueMin() != null) {
                w.ge(GiftCardCashActivity::getFaceValue, params.getFaceValueMin());
            }
            if (params.getFaceValueMax() != null) {
                w.le(GiftCardCashActivity::getFaceValue, params.getFaceValueMax());
            }
            if (params.getStockTotalMin() != null) {
                w.ge(GiftCardCashActivity::getStockTotal, params.getStockTotalMin());
            }
            if (params.getStockTotalMax() != null) {
                w.le(GiftCardCashActivity::getStockTotal, params.getStockTotalMax());
            }
            if (CharSequenceUtil.isNotBlank(params.getActivityStatus())) {
                String activityStatus = normalizeActivityStatus(params.getActivityStatus());
                if (activityStatus == null) {
                    throw new ServiceException(ResultCode.PARAMS_ERROR, "活动状态仅支持：ALL、NORMAL、EXPIRED");
                }
                if ("NORMAL".equals(activityStatus)) {
                    w.and(q -> q.eq(GiftCardCashActivity::getValidityType, GiftCardCashValidityTypeEnum.LONG_TERM.name())
                            .or()
                            .eq(GiftCardCashActivity::getValidityType, GiftCardCashValidityTypeEnum.AFTER_ACTIVATE_MONTHS.name())
                            .or()
                            .and(x -> x.eq(GiftCardCashActivity::getValidityType, GiftCardCashValidityTypeEnum.FIXED_UNTIL.name())
                                    .and(y -> y.isNull(GiftCardCashActivity::getValidEndTime)
                                            .or()
                                            .gt(GiftCardCashActivity::getValidEndTime, new Date()))));
                } else if ("EXPIRED".equals(activityStatus)) {
                    w.eq(GiftCardCashActivity::getValidityType, GiftCardCashValidityTypeEnum.FIXED_UNTIL.name())
                            .isNotNull(GiftCardCashActivity::getValidEndTime)
                            .le(GiftCardCashActivity::getValidEndTime, new Date());
                }
            }
        }
        w.orderByDesc(GiftCardCashActivity::getCreateTime);
        IPage<GiftCardCashActivity> result = activityMapper.selectPage(mpPage, w);
        return result.convert(this::toActivityVo);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String batchCreateCards(String activityId, GiftCardCashBatchCreateDTO dto) {
        return GiftCardCashBatchSupport.batchCreateCards(activityMapper, createBatchMapper, credentialMapper, adminUserService, activityId, dto);
    }

    @Override
    public IPage<GiftCardCashCreateBatchVO> pageCreateBatch(GiftCardCashCreateBatchSearchParams params, PageVO page) {
        return GiftCardCashBatchSupport.pageCreateBatch(createBatchMapper, params, page);
    }

    @Override
    public IPage<GiftCardCashCredentialVO> pageCredential(GiftCardCashCredentialSearchParams params, PageVO page) {
        return GiftCardCashCredentialSupport.pageCredential(credentialMapper, memberService, params, page);
    }

    @Override
    public IPage<GiftCardCashIssueBatchVO> pageIssueBatch(GiftCardCashIssueBatchSearchParams params, PageVO page) {
        return GiftCardCashBatchSupport.pageIssueBatch(issueBatchMapper, params, page);
    }

    @Override
    public IPage<GiftCardUsageRecordVO> pageUsageRecord(GiftCardUsageRecordSearchParams params, PageVO page) {
        return GiftCardCashTradeSupport.pageUsageRecord(orderGiftCardRecordMapper, params, page);
    }

    @Override
    public void exportCreateBatch(String createBatchId, HttpServletResponse response) {
        GiftCardCashBatchSupport.exportCreateBatch(createBatchMapper, credentialMapper, activityMapper, createBatchId, response);
    }

    @Override
    public void downloadIssueTemplate(HttpServletResponse response) {
        GiftCardCashBatchSupport.downloadIssueTemplate(response);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String issueImportByActivity(String activityId, MultipartFile file) {
        return GiftCardCashBatchSupport.issueImportByActivity(activityMapper, issueBatchMapper, credentialMapper, memberService, adminUserService, activityId, file);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int expireOverdueCredentials() {
        return GiftCardCashCredentialSupport.expireOverdueCredentials(credentialMapper, activityMapper);
    }

    @Override
    public IPage<GiftCardCashMemberCardVO> pageMemberCards(String memberId, GiftCardCashMemberCardSearchParams params, PageVO page) {
        return GiftCardCashCredentialSupport.pageMemberCards(credentialMapper, activityMapper, memberId, params, page);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void bindMemberCard(String memberId, String cardNo, String cardSecret) {
        GiftCardCashCredentialSupport.bindMemberCard(credentialMapper, activityMapper, memberId, cardNo, cardSecret);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void activateMemberCard(String memberId, String memberCardId) {
        GiftCardCashCredentialSupport.activateMemberCard(credentialMapper, activityMapper, orderGiftCardRecordMapper, memberId, memberCardId);
    }

    @Override
    public List<GiftCardCashMemberCardVO> listAvailableMemberCards(String memberId) {
        return GiftCardCashCredentialSupport.listAvailableMemberCards(credentialMapper, activityMapper, memberId);
    }

    @Override
    public GiftCardCashDeductPreviewVO previewMemberCardDeduction(String memberId, GiftCardCashDeductPreviewDTO dto) {
        return GiftCardCashTradeSupport.previewMemberCardDeduction(credentialMapper, memberId, dto);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deductMemberCardBalance(String memberId, List<GiftCardCashDeductPreviewVO.DeductItem> deductItems) {
        GiftCardCashTradeSupport.deductMemberCardBalance(credentialMapper, memberId, deductItems);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public BigDecimal refundMemberCardBalance(AfterSale afterSale) {
        return GiftCardCashTradeSupport.refundMemberCardBalance(orderGiftCardRecordMapper, credentialMapper, afterSale);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public BigDecimal refundMemberCardBalance(AfterSale afterSale, BigDecimal refundLimit) {
        return GiftCardCashTradeSupport.refundMemberCardBalance(orderGiftCardRecordMapper, credentialMapper, afterSale, refundLimit);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public BigDecimal refundOrderMemberCardBalance(String orderSn, BigDecimal refundLimit) {
        return GiftCardCashTradeSupport.refundOrderMemberCardBalance(orderGiftCardRecordMapper, credentialMapper, orderSn, refundLimit);
    }

    @Override
    public BigDecimal resolveChannelRefundAfterGiftCard(String orderSn, BigDecimal orderAmount) {
        return GiftCardCashTradeSupport.resolveChannelRefundAfterGiftCard(orderGiftCardRecordMapper, credentialMapper, orderSn, orderAmount);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public BigDecimal resolveChannelRefundAfterGiftCardForAfterSale(AfterSale afterSale) {
        return GiftCardCashTradeSupport.resolveChannelRefundAfterGiftCardForAfterSale(orderGiftCardRecordMapper, credentialMapper, orderItemService, afterSale);
    }

    @Override
    public void applyGiftCardUsageOnOrderCreate(TradeDTO tradeDTO, List<Order> orders, List<OrderItem> orderItems) {
        GiftCardCashTradeSupport.applyGiftCardUsageOnOrderCreate(credentialMapper, orderGiftCardRecordMapper, tradeDTO, orders, orderItems);
    }

    /** 规范化活动状态入参，仅支持英文。 */
    private String normalizeActivityStatus(String rawStatus) {
        if (CharSequenceUtil.isBlank(rawStatus)) {
            return null;
        }
        String status = rawStatus.trim();
        if ("ALL".equalsIgnoreCase(status)) {
            return "ALL";
        }
        if ("NORMAL".equalsIgnoreCase(status)) {
            return "NORMAL";
        }
        if ("EXPIRED".equalsIgnoreCase(status)) {
            return "EXPIRED";
        }
        return null;
    }

    /**
     * 将持久化活动转换为列表和详情共用的展示对象，并统计未发放、导入发卡及剩余库存。
     *
     * @param activity 数据库读取的活动实体
     * @return 含库存统计的活动展示数据，不修改持久化实体
     */
    private GiftCardCashActivityVO toActivityVo(GiftCardCashActivity activity) {
        GiftCardCashActivityVO vo = new GiftCardCashActivityVO();
        BeanUtils.copyProperties(activity, vo);
        long unissuedPool = countUnissuedByActivity(activity.getId());
        long issuedByImport = countIssuedByIssueBatch(activity.getId());
        long used = countCredentialsByActivity(activity.getId());
        vo.setProducedQuantity((int) unissuedPool);
        vo.setIssuedCardQuantity((int) issuedByImport);
        int stockTotal = activity.getStockTotal() == null ? 0 : activity.getStockTotal();
        vo.setRemainingStock(stockTotal - (int) used);
        return vo;
    }

    /** 统计活动下总卡密数量（含已发、未发，不含逻辑删除）。 */
    private long countCredentialsByActivity(String activityId) {
        return credentialMapper.selectCount(new LambdaQueryWrapper<cn.lili.modules.promotion.entity.dos.GiftCardCashCredential>()
                .eq(cn.lili.modules.promotion.entity.dos.GiftCardCashCredential::getActivityId, activityId)
                .eq(cn.lili.modules.promotion.entity.dos.GiftCardCashCredential::getDeleteFlag, false));
    }

    /** 统计活动下未发放卡密数量。 */
    private long countUnissuedByActivity(String activityId) {
        return credentialMapper.selectCount(new LambdaQueryWrapper<cn.lili.modules.promotion.entity.dos.GiftCardCashCredential>()
                .eq(cn.lili.modules.promotion.entity.dos.GiftCardCashCredential::getActivityId, activityId)
                .eq(cn.lili.modules.promotion.entity.dos.GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.UNISSUED.name())
                .eq(cn.lili.modules.promotion.entity.dos.GiftCardCashCredential::getDeleteFlag, false));
    }

    /** 统计活动下导入发卡数量（有发卡批次ID的凭证）。 */
    private long countIssuedByIssueBatch(String activityId) {
        return credentialMapper.selectCount(new LambdaQueryWrapper<cn.lili.modules.promotion.entity.dos.GiftCardCashCredential>()
                .eq(cn.lili.modules.promotion.entity.dos.GiftCardCashCredential::getActivityId, activityId)
                .isNotNull(cn.lili.modules.promotion.entity.dos.GiftCardCashCredential::getIssueBatchId)
                .eq(cn.lili.modules.promotion.entity.dos.GiftCardCashCredential::getDeleteFlag, false));
    }

    /**
     * 判断活动核心规则是否一致。
     * 核心规则用于限制“产卡后不可修改”的字段。
     */
    private boolean activityCoreEquals(GiftCardCashActivity a, GiftCardCashActivityDTO d) {
        if (!Objects.equals(a.getPickupMode(), d.getPickupMode()) || !Objects.equals(a.getPickupCount(), d.getPickupCount())
                || !Objects.equals(a.getPickupSkuIds(), d.getPickupSkuIds() == null ? null : com.alibaba.fastjson2.JSON.toJSONString(d.getPickupSkuIds()))) {
            return false;
        }
        if (a.getFaceValue().compareTo(d.getFaceValue().setScale(2, RoundingMode.HALF_UP)) != 0) {
            return false;
        }
        if (!Objects.equals(a.getCardType(), d.getCardType())) {
            return false;
        }
        if (!Objects.equals(a.getValidityType(), d.getValidityType())) {
            return false;
        }
        Date endA = a.getValidEndTime();
        Date endD = d.getValidEndTime();
        if (endA == null ^ endD == null) {
            return false;
        }
        if (endA != null && endA.getTime() != endD.getTime()) {
            return false;
        }
        return Objects.equals(a.getValidMonthsAfterActivate(), d.getValidMonthsAfterActivate());
    }

    /** 将活动DTO中的可落库字段复制到活动实体。 */
    private void copyDtoToActivity(GiftCardCashActivity entity, GiftCardCashActivityDTO dto) {
        entity.setGiftCardName(dto.getGiftCardName().trim());
        entity.setCardType(dto.getCardType());
        entity.setPickupMode(dto.getPickupMode());
        entity.setPickupCount(dto.getPickupCount());
        entity.setPickupSkuIds(dto.getPickupSkuIds() == null ? null : com.alibaba.fastjson2.JSON.toJSONString(dto.getPickupSkuIds()));
        entity.setFaceValue(dto.getFaceValue().setScale(2, RoundingMode.HALF_UP));
        entity.setValidityType(dto.getValidityType());
        entity.setValidEndTime(dto.getValidEndTime());
        entity.setValidMonthsAfterActivate(dto.getValidMonthsAfterActivate());
        entity.setStockTotal(dto.getStockTotal());
    }

    /**
     * 规范化并校验活动参数：
     * 1) 按 CASH / PICKUP 分别校验金额或兑换商品规则
     * 2) 校验有效期类型及其配套字段
     * 3) 对不适用字段做清理，避免脏数据写入
     */
    private void normalizeActivityDto(GiftCardCashActivityDTO dto) {
        String raw = CharSequenceUtil.isBlank(dto.getCardType())
                ? GiftCardTypeEnum.CASH.name() : dto.getCardType().trim();
        GiftCardTypeEnum cardTypeEnum;
        try {
            cardTypeEnum = GiftCardTypeEnum.valueOf(raw);
        } catch (IllegalArgumentException ex) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡类型无效，请使用 CASH（现金卡）或 PICKUP（提货卡）");
        }
        dto.setCardType(cardTypeEnum.name());
        if (cardTypeEnum == GiftCardTypeEnum.PICKUP) {
            pickupCardRuleService.validateActivity(dto);
        } else {
            if (dto.getFaceValue() == null || dto.getFaceValue().signum() <= 0 || dto.getFaceValue().scale() > 2) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "现金卡面额必须大于零且最多两位小数");
            }
            dto.setPickupMode(null);
            dto.setPickupCount(null);
            dto.setPickupSkuIds(null);
        }
        String validityType = CharSequenceUtil.isBlank(dto.getValidityType())
                ? GiftCardCashValidityTypeEnum.LONG_TERM.name() : dto.getValidityType().trim();
        try {
            GiftCardCashValidityTypeEnum.valueOf(validityType);
        } catch (IllegalArgumentException ex) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "有效期类型无效");
        }
        dto.setValidityType(validityType);
        if (GiftCardCashValidityTypeEnum.FIXED_UNTIL.name().equals(validityType)) {
            if (dto.getValidEndTime() == null) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "固定到期模式必须填写 validEndTime");
            }
            Date now = new Date();
            if (!dto.getValidEndTime().after(now)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "固定到期时间须晚于当前时间");
            }
        } else if (GiftCardCashValidityTypeEnum.AFTER_ACTIVATE_MONTHS.name().equals(validityType)) {
            Integer m = dto.getValidMonthsAfterActivate();
            if (m == null || m < 1 || m > 99) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "激活后有效月数须在 1-99");
            }
        } else {
            dto.setValidEndTime(null);
            dto.setValidMonthsAfterActivate(null);
        }
    }

    /** 活动行锁统一串行化制卡、发卡及修改，防止超发和已发权益被修改。 */
    private GiftCardCashActivity lockActivity(String id) {
        GiftCardCashActivity activity = activityMapper.selectOne(new LambdaQueryWrapper<GiftCardCashActivity>()
                .eq(GiftCardCashActivity::getId, id).eq(GiftCardCashActivity::getDeleteFlag, false).last("FOR UPDATE"));
        if (activity == null) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_ACTIVITY_NOT_FOUND);
        }
        return activity;
    }

    private GiftCardCashActivity getActivityOrThrow(String id) {
        GiftCardCashActivity a = activityMapper.selectById(id);
        if (a == null || Boolean.TRUE.equals(a.getDeleteFlag())) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_ACTIVITY_NOT_FOUND);
        }
        return a;
    }
}
