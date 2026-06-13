package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.convert.Convert;
import cn.hutool.core.io.IoUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.RandomUtil;
import cn.hutool.poi.excel.ExcelReader;
import cn.hutool.poi.excel.ExcelUtil;
import cn.hutool.poi.excel.ExcelWriter;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.permission.entity.dos.AdminUser;
import cn.lili.modules.permission.service.AdminUserService;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.OrderGiftCardRecord;
import cn.lili.modules.order.order.mapper.OrderGiftCardRecordMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.promotion.entity.dos.GiftCardCashActivity;
import cn.lili.modules.promotion.entity.dos.GiftCardCashCreateBatch;
import cn.lili.modules.promotion.entity.dos.GiftCardCashCredential;
import cn.lili.modules.promotion.entity.dos.GiftCardCashIssueBatch;
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
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.vos.GiftCardCashCredentialVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashCreateBatchVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashDeductPreviewVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashIssueBatchVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashMemberCardVO;
import cn.lili.modules.promotion.entity.vos.GiftCardUsageRecordVO;
import cn.lili.modules.promotion.mapper.GiftCardCashActivityMapper;
import cn.lili.modules.promotion.mapper.GiftCardCashCreateBatchMapper;
import cn.lili.modules.promotion.mapper.GiftCardCashCredentialMapper;
import cn.lili.modules.promotion.mapper.GiftCardCashIssueBatchMapper;
import cn.lili.modules.promotion.service.GiftCardCashService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 现金礼品卡服务实现类（活动、制卡、发卡、绑定、抵扣、退款等核心业务）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Slf4j
@Service
public class GiftCardCashServiceImpl implements GiftCardCashService {
    /** 单次导入发卡允许的最大数据行数（不含表头）。 */
    private static final int MAX_ISSUE_ROWS = 1000;

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


    @Override
    @Transactional(rollbackFor = Exception.class)
    /** 创建礼品卡活动，并返回活动ID。 */
    public String saveActivity(GiftCardCashActivityDTO dto) {
        normalizeActivityDto(dto);
        String id = SnowFlake.getIdStr();
        GiftCardCashActivity entity = new GiftCardCashActivity();
        entity.setId(id);
        copyDtoToActivity(entity, dto);
        activityMapper.insert(entity);
        return id;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    /**
     * 更新礼品卡活动。
     * 已经产卡后，只允许改活动名称和总库存，且库存不能小于已产数量。
     */
    public void updateActivity(String id, GiftCardCashActivityDTO dto) {
        GiftCardCashActivity current = getActivityOrThrow(id);
        normalizeActivityDto(dto);
        long produced = countCredentialsByActivity(id);
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
    /** 删除活动：仅在未产生卡密且未产生制卡批次时允许删除。 */
    public void deleteActivity(String id) {
        getActivityOrThrow(id);
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
    /** 查询活动详情并回填聚合统计（已产、已发、剩余库存）。 */
    public GiftCardCashActivity getActivity(String id) {
        GiftCardCashActivity a = getActivityOrThrow(id);
        fillActivityAggregateSingle(a);
        return a;
    }

    @Override
    /** 分页查询活动列表，并补充每条活动的聚合统计字段。 */
    public IPage<GiftCardCashActivity> pageActivity(GiftCardCashActivitySearchParams params, PageVO page) {
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
        fillActivityAggregates(result);
        return result;
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

    private String normalizeUsageTransactionType(GiftCardUsageRecordSearchParams params) {
        if (params == null) {
            return null;
        }
        String type = CharSequenceUtil.isNotBlank(params.getTransactionType())
                ? params.getTransactionType().trim()
                : CharSequenceUtil.trim(params.getBizType());
        if (CharSequenceUtil.isBlank(type)) {
            return null;
        }
        if ("ORDER_DEDUCT".equalsIgnoreCase(type)) {
            return "ORDER_DEDUCT";
        }
        if ("ORDER_REFUND".equalsIgnoreCase(type)) {
            return "ORDER_REFUND";
        }
        if ("ORDER_CANCEL".equalsIgnoreCase(type)) {
            return "ORDER_CANCEL";
        }
        return type;
    }

    /** 给分页活动列表批量填充聚合字段。 */
    private void fillActivityAggregates(IPage<GiftCardCashActivity> page) {
        if (page == null || CollUtil.isEmpty(page.getRecords())) {
            return;
        }
        for (GiftCardCashActivity a : page.getRecords()) {
            fillActivityAggregateSingle(a);
        }
    }

    /** 给单个活动填充聚合字段。 */
    private void fillActivityAggregateSingle(GiftCardCashActivity a) {
        long unissuedPool = countUnissuedByActivity(a.getId());
        long issuedByImport = countIssuedByIssueBatch(a.getId());
        long used = countCredentialsByActivity(a.getId());
        a.setProducedQuantity((int) unissuedPool);
        a.setIssuedCardQuantity((int) issuedByImport);
        int stockTotal = a.getStockTotal() == null ? 0 : a.getStockTotal();
        a.setRemainingStock(stockTotal - (int) used);
    }

    /** 统计活动下总卡密数量（含已发、未发，不含逻辑删除）。 */
    private long countCredentialsByActivity(String activityId) {
        return credentialMapper.selectCount(new LambdaQueryWrapper<GiftCardCashCredential>()
                .eq(GiftCardCashCredential::getActivityId, activityId)
                .eq(GiftCardCashCredential::getDeleteFlag, false));
    }

    /** 统计活动下未发放卡密数量。 */
    private long countUnissuedByActivity(String activityId) {
        return credentialMapper.selectCount(new LambdaQueryWrapper<GiftCardCashCredential>()
                .eq(GiftCardCashCredential::getActivityId, activityId)
                .eq(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.UNISSUED.name())
                .eq(GiftCardCashCredential::getDeleteFlag, false));
    }

    /** 统计活动下导入发卡数量（有发卡批次ID的凭证）。 */
    private long countIssuedByIssueBatch(String activityId) {
        return credentialMapper.selectCount(new LambdaQueryWrapper<GiftCardCashCredential>()
                .eq(GiftCardCashCredential::getActivityId, activityId)
                .isNotNull(GiftCardCashCredential::getIssueBatchId)
                .eq(GiftCardCashCredential::getDeleteFlag, false));
    }

    /** 活动剩余可占用库存（制卡、发卡共用）。 */
    private int remainingActivityStock(String activityId) {
        GiftCardCashActivity activity = getActivityOrThrow(activityId);
        int stockTotal = activity.getStockTotal() == null ? 0 : activity.getStockTotal();
        return stockTotal - (int) countCredentialsByActivity(activityId);
    }

    /**
     * 判断活动核心规则是否一致。
     * 核心规则用于限制“产卡后不可修改”的字段。
     */
    private boolean activityCoreEquals(GiftCardCashActivity a, GiftCardCashActivityDTO d) {
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
        entity.setFaceValue(dto.getFaceValue().setScale(2, RoundingMode.HALF_UP));
        entity.setValidityType(dto.getValidityType());
        entity.setValidEndTime(dto.getValidEndTime());
        entity.setValidMonthsAfterActivate(dto.getValidMonthsAfterActivate());
        entity.setStockTotal(dto.getStockTotal());
    }

    /**
     * 规范化并校验活动参数：
     * 1) 仅允许现金卡CASH
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
            throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡类型无效，请使用 CASH（现金卡）或 PICKUP（提货卡，尚未开放）");
        }
        if (cardTypeEnum != GiftCardTypeEnum.CASH) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "本期仅支持现金卡（CASH），提货卡（PICKUP）功能尚未开放");
        }
        dto.setCardType(GiftCardTypeEnum.CASH.name());
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

    @Override
    @Transactional(rollbackFor = Exception.class)
    /** 批量制卡：校验库存后创建制卡批次，并生成对应数量的卡密凭证。 */
    public String batchCreateCards(String activityId, GiftCardCashBatchCreateDTO dto) {
        int quantity = dto.getQuantity();
        if (quantity < 1 || quantity > 10000) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_CREATE_QUANTITY_ERROR);
        }
        GiftCardCashActivity activity = getActivityOrThrow(activityId);
        long produced = countCredentialsByActivity(activityId);
        if (produced + quantity > activity.getStockTotal()) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_ACTIVITY_OVER_STOCK);
        }
        BigDecimal face = activity.getFaceValue().setScale(2, RoundingMode.HALF_UP);
        String batchRemark = CharSequenceUtil.blankToDefault(CharSequenceUtil.trim(dto.getBatchRemark()), null);
        String batchId = SnowFlake.getIdStr();
        GiftCardCashCreateBatch batch = new GiftCardCashCreateBatch();
        batch.setId(batchId);
        batch.setActivityId(activityId);
        batch.setQuantity(quantity);
        batch.setBatchRemark(batchRemark);
        setCreateBatchOperator(batch);
        createBatchMapper.insert(batch);
        for (int i = 0; i < quantity; i++) {
            GiftCardCashCredential cred = new GiftCardCashCredential();
            cred.setId(SnowFlake.getIdStr());
            cred.setActivityId(activityId);
            cred.setCreateBatchId(batchId);
            // 卡号按批次内递增生成：保证可读顺序；batchId 唯一保证跨批次不重复
            cred.setCardNo(buildSequentialCardNo(batchId, i + 1));
            cred.setCardSecret(RandomUtil.randomStringUpper(12));
            cred.setFaceValue(face);
            cred.setBalance(face);
            cred.setStatus(GiftCardCashCredentialStatusEnum.UNISSUED.name());
            credentialMapper.insert(cred);
        }
        return batchId;
    }

    @Override
    /** 分页查询制卡批次。 */
    public IPage<GiftCardCashCreateBatchVO> pageCreateBatch(GiftCardCashCreateBatchSearchParams params, PageVO page) {
        Page<GiftCardCashCreateBatchVO> mpPage = PageUtil.initPage(page);
        QueryWrapper<GiftCardCashCreateBatchVO> w = new QueryWrapper<>();
        w.eq("cb.delete_flag", false);
        if (params != null) {
            if (CharSequenceUtil.isNotBlank(params.getActivityId())) {
                w.eq("cb.activity_id", params.getActivityId().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getBatchRemark())) {
                w.like("cb.batch_name", params.getBatchRemark().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getCreateBatchId())) {
                w.eq("cb.id", params.getCreateBatchId().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getCreateUserName())) {
                w.like("cb.create_user_name", params.getCreateUserName().trim());
            }
            if (params.getCreateTimeStart() != null) {
                w.ge("cb.create_time", params.getCreateTimeStart());
            }
            if (params.getCreateTimeEnd() != null) {
                w.le("cb.create_time", params.getCreateTimeEnd());
            }
            boolean needCredentialExists = CharSequenceUtil.isNotBlank(params.getCardNo())
                    || params.getExpireTimeStart() != null
                    || params.getExpireTimeEnd() != null;
            if (needCredentialExists) {
                StringBuilder existsSql = new StringBuilder();
                existsSql.append("SELECT 1 FROM li_gift_card_cash_credential c ")
                        .append("WHERE c.delete_flag = 0 ")
                        .append("AND c.create_batch_id = cb.id");
                if (CharSequenceUtil.isNotBlank(params.getCardNo())) {
                    String cardNo = params.getCardNo().trim().replace("'", "''");
                    existsSql.append(" AND c.card_no LIKE '%").append(cardNo).append("%'");
                }
                if (params.getExpireTimeStart() != null) {
                    existsSql.append(" AND c.expire_time >= '")
                            .append(formatDateTimeForSql(params.getExpireTimeStart()))
                            .append("'");
                }
                if (params.getExpireTimeEnd() != null) {
                    existsSql.append(" AND c.expire_time <= '")
                            .append(formatDateTimeForSql(params.getExpireTimeEnd()))
                            .append("'");
                }
                w.exists(existsSql.toString());
            }
        }
        w.orderByDesc("cb.create_time");
        return createBatchMapper.pageByParams(mpPage, w);
    }

    @Override
    /**
     * 分页查询卡密凭证。
     * 支持按会员手机号筛选，内部会先解析会员ID再构造in查询。
     */
    public IPage<GiftCardCashCredentialVO> pageCredential(GiftCardCashCredentialSearchParams params, PageVO page) {
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
        return raw.convert(this::toCredentialVo);
    }

    /** 将卡密实体转换为展示VO，并补齐会员手机号/昵称。 */
    private GiftCardCashCredentialVO toCredentialVo(GiftCardCashCredential c) {
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

    @Override
    /** 分页查询发卡批次记录。 */
    public IPage<GiftCardCashIssueBatchVO> pageIssueBatch(GiftCardCashIssueBatchSearchParams params, PageVO page) {
        Page<GiftCardCashIssueBatchVO> mpPage = PageUtil.initPage(page);
        QueryWrapper<GiftCardCashIssueBatchVO> w = new QueryWrapper<>();
        w.eq("ib.delete_flag", false);
        if (params != null) {
            if (CharSequenceUtil.isNotBlank(params.getActivityId())) {
                w.eq("ib.activity_id", params.getActivityId().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getCreateBatchId())) {
                w.eq("ib.create_batch_id", params.getCreateBatchId().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getIssueBatchId())) {
                w.eq("ib.id", params.getIssueBatchId().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getIssueUser())) {
                String issueUser = params.getIssueUser().trim();
                w.and(q -> q.like("ib.issue_user_name", issueUser)
                        .or()
                        .like("ib.issue_user_mobile", issueUser));
            }
            if (params.getIssueTimeStart() != null) {
                w.ge("ib.create_time", params.getIssueTimeStart());
            }
            if (params.getIssueTimeEnd() != null) {
                w.le("ib.create_time", params.getIssueTimeEnd());
            }
            boolean needCredentialExists = CharSequenceUtil.isNotBlank(params.getCardNo())
                    || params.getExpireTimeStart() != null
                    || params.getExpireTimeEnd() != null;
            if (needCredentialExists) {
                StringBuilder existsSql = new StringBuilder();
                existsSql.append("SELECT 1 FROM li_gift_card_cash_credential c ")
                        .append("WHERE c.delete_flag = 0 ")
                        .append("AND c.issue_batch_id = ib.id");
                if (CharSequenceUtil.isNotBlank(params.getCardNo())) {
                    String cardNo = params.getCardNo().trim().replace("'", "''");
                    existsSql.append(" AND c.card_no LIKE '%").append(cardNo).append("%'");
                }
                if (params.getExpireTimeStart() != null) {
                    String start = formatDateTimeForSql(params.getExpireTimeStart());
                    existsSql.append(" AND c.expire_time >= '").append(start).append("'");
                }
                if (params.getExpireTimeEnd() != null) {
                    String end = formatDateTimeForSql(params.getExpireTimeEnd());
                    existsSql.append(" AND c.expire_time <= '").append(end).append("'");
                }
                w.exists(existsSql.toString());
            }
        }
        w.orderByDesc("ib.create_time");
        return issueBatchMapper.pageByParams(mpPage, w);
    }

    @Override
    public IPage<GiftCardUsageRecordVO> pageUsageRecord(GiftCardUsageRecordSearchParams params, PageVO page) {
        Page<GiftCardUsageRecordVO> mpPage = PageUtil.initPage(page);
        QueryWrapper<GiftCardUsageRecordVO> w = new QueryWrapper<>();
        w.eq("ogr.delete_flag", false);
        if (params != null) {
            if (CharSequenceUtil.isNotBlank(params.getOrderSn())) {
                w.eq("ogr.order_sn", params.getOrderSn().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getTradeSn())) {
                w.eq("ogr.trade_sn", params.getTradeSn().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getMemberId())) {
                w.eq("ogr.member_id", params.getMemberId().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getMemberMobile())) {
                w.eq("m.mobile", params.getMemberMobile().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getMemberName())) {
                String memberName = params.getMemberName().trim();
                w.and(q -> q.like("m.nick_name", memberName).or().like("m.username", memberName));
            }
            if (CharSequenceUtil.isNotBlank(params.getCardNo())) {
                w.like("ogr.card_no", params.getCardNo().trim());
            }
            if (CharSequenceUtil.isNotBlank(params.getFlowType())) {
                w.eq("ogr.flow_type", params.getFlowType().trim());
            }
            String transactionType = normalizeUsageTransactionType(params);
            if (CharSequenceUtil.isNotBlank(transactionType)) {
                w.eq("ogr.biz_type", transactionType);
            }
            if (CharSequenceUtil.isNotBlank(params.getBizSn())) {
                w.eq("ogr.biz_sn", params.getBizSn().trim());
            }
            if (params.getTransactionTimeStart() != null) {
                w.ge("ogr.create_time", params.getTransactionTimeStart());
            }
            if (params.getTransactionTimeEnd() != null) {
                w.le("ogr.create_time", params.getTransactionTimeEnd());
            }
        }
        w.orderByDesc("ogr.create_time");
        return orderGiftCardRecordMapper.pageUsageRecord(mpPage, w);
    }

    @Override
    /** 导出制卡批次下全部卡密（卡号、卡密、面值、礼品卡名称等）。 */
    public void exportCreateBatch(String createBatchId, HttpServletResponse response) {
        GiftCardCashCreateBatch batch = getCreateBatchOrThrow(createBatchId);
        List<GiftCardCashCredential> list = credentialMapper.selectList(new LambdaQueryWrapper<GiftCardCashCredential>()
                .eq(GiftCardCashCredential::getCreateBatchId, createBatchId)
                .eq(GiftCardCashCredential::getDeleteFlag, false)
                .orderByAsc(GiftCardCashCredential::getCardNo));
        GiftCardCashActivity activity = getActivityOrThrow(batch.getActivityId());
        String batchLabel = CharSequenceUtil.blankToDefault(activity.getGiftCardName(), "-");
        ExcelWriter writer = ExcelUtil.getWriter(true);
        writer.writeHeadRow(CollUtil.newArrayList("卡号", "卡密", "面值", "礼品卡名称", "制卡批次ID"));
        for (GiftCardCashCredential cred : list) {
            writer.writeRow(CollUtil.newArrayList(
                    cred.getCardNo(),
                    cred.getCardSecret(),
                    cred.getFaceValue().toPlainString(),
                    batchLabel,
                    createBatchId
            ));
        }
        ServletOutputStream out = null;
        try {
            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;charset=utf-8");
            String fn = URLEncoder.encode("制卡导出_" + createBatchId, StandardCharsets.UTF_8);
            response.setHeader("Content-Disposition", "attachment;filename=" + fn + ".xlsx");
            out = response.getOutputStream();
            writer.flush(out, true);
        } catch (Exception e) {
            log.error("exportCreateBatch", e);
            throw new ServiceException(ResultCode.ERROR);
        } finally {
            writer.close();
            IoUtil.close(out);
        }
    }

    @Override
    /** 下载批量发卡导入模板（手机号、发卡数量）。 */
    public void downloadIssueTemplate(HttpServletResponse response) {
        ExcelWriter writer = ExcelUtil.getWriter(true);
        writer.writeHeadRow(CollUtil.newArrayList("手机号", "发卡数量"));
        ServletOutputStream out = null;
        try {
            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;charset=utf-8");
            String fn = URLEncoder.encode("批量发卡导入模板", StandardCharsets.UTF_8);
            response.setHeader("Content-Disposition", "attachment;filename=" + fn + ".xlsx");
            out = response.getOutputStream();
            writer.flush(out, true);
        } catch (Exception e) {
            log.error("downloadIssueTemplate", e);
            throw new ServiceException(ResultCode.ERROR);
        } finally {
            writer.close();
            IoUtil.close(out);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    /** 按活动导入发卡：直接生成凭证并发放给会员，扣减活动库存（不依赖制卡池）。 */
    public String issueImportByActivity(String activityId, MultipartFile file) {
        getActivityOrThrow(activityId);
        return runIssueImport(activityId, file);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int expireOverdueCredentials() {
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

    @Override
    /**
     * 买家端分页查询会员礼品卡。
     * <p>
     * 状态筛选规则：
     * <ul>
     *   <li>PENDING_ACTIVATION：绑定会员=当前会员，且激活会员为空；</li>
     *   <li>AVAILABLE：可用礼品卡=当前会员，余额大于0，且未过有效期；</li>
     *   <li>UNAVAILABLE：不可用礼品卡=当前会员，且余额为0/空或已过有效期。</li>
     * </ul>
     * 未传状态时，按绑定会员（memberId）查询当前会员的全部礼品卡。
     */
    public IPage<GiftCardCashMemberCardVO> pageMemberCards(String memberId, GiftCardCashMemberCardSearchParams params, PageVO page) {
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
                        .gt(GiftCardCashCredential::getBalance, BigDecimal.ZERO)
                        .and(q -> q.isNull(GiftCardCashCredential::getExpireTime)
                                .or()
                                .gt(GiftCardCashCredential::getExpireTime, now));
            } else {
                w.eq(GiftCardCashCredential::getActivateMemberId, memberId)
                        .and(q -> q.isNull(GiftCardCashCredential::getBalance)
                                .or()
                                .le(GiftCardCashCredential::getBalance, BigDecimal.ZERO)
                                .or(x -> x.isNotNull(GiftCardCashCredential::getExpireTime)
                                        .le(GiftCardCashCredential::getExpireTime, now)));
            }
        } else {
            w.eq(GiftCardCashCredential::getMemberId, memberId);
        }
        w.orderByDesc(GiftCardCashCredential::getCreateTime);
        IPage<GiftCardCashCredential> raw = credentialMapper.selectPage(mpPage, w);
        return raw.convert(this::toMemberCardVo);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void bindMemberCard(String memberId, String cardNo, String cardSecret) {
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
        GiftCardCashActivity activity = getActivityOrThrow(credential.getActivityId());
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

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void activateMemberCard(String memberId, String memberCardId) {
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
        GiftCardCashActivity activity = getActivityOrThrow(credential.getActivityId());
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
        if (refreshed != null) {
            saveGiftCardUsageRecord(null, null, memberId, refreshed, refreshed.getFaceValue(),
                    "INCREASE", "CARD_ACTIVATE", memberCardId, refreshed.getBalance());
        }
    }

    @Override
    public List<GiftCardCashMemberCardVO> listAvailableMemberCards(String memberId) {
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
        return list.stream().map(this::toMemberCardVo).collect(Collectors.toList());
    }

    @Override
    public GiftCardCashDeductPreviewVO previewMemberCardDeduction(String memberId, GiftCardCashDeductPreviewDTO dto) {
        if (dto == null || dto.getOrderAmount() == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "试算参数错误，orderAmount不能为空");
        }
        BigDecimal orderAmount = dto.getOrderAmount().setScale(2, RoundingMode.HALF_UP);
        if (orderAmount.compareTo(BigDecimal.ZERO) < 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "订单金额不能小于0");
        }
        GiftCardCashDeductPreviewVO result = new GiftCardCashDeductPreviewVO();
        result.setOrderAmount(orderAmount);
        result.setGiftCardDeductAmount(zeroMoney());
        result.setCashPayAmount(orderAmount);
        if (CollUtil.isEmpty(dto.getCredentialIds()) || orderAmount.compareTo(BigDecimal.ZERO) == 0) {
            return result;
        }
        List<String> credentialIds = new ArrayList<>(new LinkedHashSet<>(dto.getCredentialIds()));
        Date now = new Date();
        List<GiftCardCashCredential> selected = credentialMapper.selectList(new LambdaQueryWrapper<GiftCardCashCredential>()
                .in(GiftCardCashCredential::getId, credentialIds)
                .eq(GiftCardCashCredential::getDeleteFlag, false)
                .eq(GiftCardCashCredential::getActivateMemberId, memberId)
                .eq(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.BOUND.name()));
        if (selected.size() != credentialIds.size()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "存在无效礼品卡，无法试算");
        }
        Map<String, GiftCardCashCredential> credentialMap = selected.stream()
                .collect(Collectors.toMap(GiftCardCashCredential::getId, c -> c));
        BigDecimal remainNeed = orderAmount;
        List<GiftCardCashDeductPreviewVO.DeductItem> items = new ArrayList<>();
        for (String credentialId : credentialIds) {
            GiftCardCashCredential credential = credentialMap.get(credentialId);
            if (credential == null) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "存在无效礼品卡，无法试算");
            }
            if (credential.getExpireTime() != null && !credential.getExpireTime().after(now)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "存在已过期礼品卡，无法试算");
            }
            BigDecimal balance = credential.getBalance() == null
                    ? zeroMoney() : credential.getBalance().setScale(2, RoundingMode.HALF_UP);
            if (balance.compareTo(BigDecimal.ZERO) <= 0 || remainNeed.compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            BigDecimal deduct = balance.min(remainNeed).setScale(2, RoundingMode.HALF_UP);
            if (deduct.compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            GiftCardCashDeductPreviewVO.DeductItem item = new GiftCardCashDeductPreviewVO.DeductItem();
            item.setCredentialId(credential.getId());
            item.setCardNo(credential.getCardNo());
            item.setDeductAmount(deduct);
            items.add(item);
            remainNeed = remainNeed.subtract(deduct).setScale(2, RoundingMode.HALF_UP);
        }
        BigDecimal giftCardDeductAmount = orderAmount.subtract(remainNeed).setScale(2, RoundingMode.HALF_UP);
        result.setDeductItems(items);
        result.setGiftCardDeductAmount(giftCardDeductAmount);
        result.setCashPayAmount(orderAmount.subtract(giftCardDeductAmount).setScale(2, RoundingMode.HALF_UP));
        return result;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deductMemberCardBalance(String memberId, List<GiftCardCashDeductPreviewVO.DeductItem> deductItems) {
        if (CollUtil.isEmpty(deductItems)) {
            return;
        }
        Date now = new Date();
        for (GiftCardCashDeductPreviewVO.DeductItem item : deductItems) {
            if (item == null || CharSequenceUtil.isBlank(item.getCredentialId()) || item.getDeductAmount() == null) {
                continue;
            }
            BigDecimal needDeduct = item.getDeductAmount().setScale(2, RoundingMode.HALF_UP);
            if (needDeduct.compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            GiftCardCashCredential credential = credentialMapper.selectById(item.getCredentialId());
            if (credential == null || Boolean.TRUE.equals(credential.getDeleteFlag())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡不存在或已失效");
            }
            if (!Objects.equals(memberId, credential.getActivateMemberId())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡归属校验失败");
            }
            if (!GiftCardCashCredentialStatusEnum.BOUND.name().equals(credential.getStatus())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡状态不可用");
            }
            if (credential.getExpireTime() != null && !credential.getExpireTime().after(now)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡已过期");
            }
            BigDecimal balance = credential.getBalance() == null
                    ? zeroMoney() : credential.getBalance().setScale(2, RoundingMode.HALF_UP);
            if (balance.compareTo(needDeduct) < 0) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡余额不足，请刷新后重试");
            }
            BigDecimal newBalance = balance.subtract(needDeduct).setScale(2, RoundingMode.HALF_UP);
            int updated = credentialMapper.update(null, new LambdaUpdateWrapper<GiftCardCashCredential>()
                    .set(GiftCardCashCredential::getBalance, newBalance)
                    .eq(GiftCardCashCredential::getId, credential.getId())
                    .eq(GiftCardCashCredential::getDeleteFlag, false)
                    .eq(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.BOUND.name())
                    .eq(GiftCardCashCredential::getActivateMemberId, memberId)
                    .eq(GiftCardCashCredential::getBalance, balance));
            if (updated != 1) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡扣减失败，请稍后重试");
            }
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public BigDecimal refundMemberCardBalance(AfterSale afterSale) {
        BigDecimal refundLimit = afterSale == null || afterSale.getActualRefundPrice() == null
                ? zeroMoney() : BigDecimal.valueOf(afterSale.getActualRefundPrice()).setScale(2, RoundingMode.HALF_UP);
        return refundMemberCardBalance(afterSale, refundLimit);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public BigDecimal refundMemberCardBalance(AfterSale afterSale, BigDecimal refundLimit) {
        if (afterSale == null || CharSequenceUtil.isBlank(afterSale.getOrderSn())) {
            return zeroMoney();
        }
        return refundOrderMemberCardBalance(afterSale.getOrderSn(), refundLimit);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public BigDecimal refundOrderMemberCardBalance(String orderSn, BigDecimal refundLimit) {
        return refundOrderMemberCardBalance(orderSn, refundLimit, "ORDER_REFUND");
    }

    @Transactional(rollbackFor = Exception.class)
    public BigDecimal refundOrderMemberCardBalance(String orderSn, BigDecimal refundLimit, String bizType) {
        if (CharSequenceUtil.isBlank(orderSn)) {
            return zeroMoney();
        }
        List<OrderGiftCardRecord> records = listOrderGiftCardRecords(orderSn);
        if (CollUtil.isEmpty(records)) {
            return zeroMoney();
        }
        BigDecimal orderGiftCardTotal = records.stream()
                .filter(record -> "DECREASE".equals(record.getFlowType()))
                .map(OrderGiftCardRecord::getDeductAmount)
                .filter(Objects::nonNull)
                .map(v -> v.setScale(2, RoundingMode.HALF_UP))
                .reduce(zeroMoney(), BigDecimal::add);
        BigDecimal refundedTotal = records.stream()
                .filter(record -> "INCREASE".equals(record.getFlowType()))
                .map(OrderGiftCardRecord::getChangeAmount)
                .filter(Objects::nonNull)
                .map(v -> v.setScale(2, RoundingMode.HALF_UP))
                .reduce(zeroMoney(), BigDecimal::add);
        BigDecimal refundableTotal = orderGiftCardTotal.subtract(refundedTotal).setScale(2, RoundingMode.HALF_UP);
        if (refundableTotal.compareTo(BigDecimal.ZERO) <= 0) {
            return zeroMoney();
        }
        BigDecimal refundRequest = refundLimit == null ? refundableTotal : refundLimit.setScale(2, RoundingMode.HALF_UP);
        if (refundRequest.compareTo(BigDecimal.ZERO) <= 0) {
            return zeroMoney();
        }
        BigDecimal shouldRefundToCard = refundRequest.min(refundableTotal).setScale(2, RoundingMode.HALF_UP);
        if (shouldRefundToCard.compareTo(BigDecimal.ZERO) <= 0) {
            return zeroMoney();
        }
        List<OrderGiftCardRecord> deductRecords = records.stream()
                .filter(record -> "DECREASE".equals(record.getFlowType()))
                .filter(record -> record.getDeductAmount() != null && record.getDeductAmount().compareTo(BigDecimal.ZERO) > 0)
                .collect(Collectors.toList());
        if (CollUtil.isEmpty(deductRecords)) {
            return zeroMoney();
        }
        BigDecimal refundedAssigned = zeroMoney();
        BigDecimal actualRefundedToCard = zeroMoney();
        Date now = new Date();
        for (int i = 0; i < deductRecords.size(); i++) {
            OrderGiftCardRecord record = deductRecords.get(i);
            BigDecimal recordDeduct = record.getDeductAmount().setScale(2, RoundingMode.HALF_UP);
            BigDecimal currentRefund;
            if (i == deductRecords.size() - 1) {
                currentRefund = shouldRefundToCard.subtract(refundedAssigned).setScale(2, RoundingMode.HALF_UP);
            } else {
                BigDecimal ratio = recordDeduct.divide(orderGiftCardTotal, 10, RoundingMode.HALF_UP);
                currentRefund = shouldRefundToCard.multiply(ratio).setScale(2, RoundingMode.HALF_UP);
                refundedAssigned = refundedAssigned.add(currentRefund);
            }
            if (currentRefund.compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            GiftCardCashCredential credential = credentialMapper.selectById(record.getCredentialId());
            if (credential == null || Boolean.TRUE.equals(credential.getDeleteFlag())) {
                continue;
            }
            BigDecimal oldBalance = credential.getBalance() == null
                    ? zeroMoney() : credential.getBalance().setScale(2, RoundingMode.HALF_UP);
            BigDecimal faceValue = credential.getFaceValue() == null
                    ? zeroMoney() : credential.getFaceValue().setScale(2, RoundingMode.HALF_UP);
            BigDecimal targetBalance = oldBalance.add(currentRefund).setScale(2, RoundingMode.HALF_UP);
            BigDecimal newBalance = targetBalance.min(faceValue);
            if (newBalance.compareTo(oldBalance) <= 0) {
                continue;
            }
            LambdaUpdateWrapper<GiftCardCashCredential> updateWrapper = new LambdaUpdateWrapper<GiftCardCashCredential>()
                    .set(GiftCardCashCredential::getBalance, newBalance)
                    .eq(GiftCardCashCredential::getId, credential.getId())
                    .eq(GiftCardCashCredential::getDeleteFlag, false)
                    .eq(GiftCardCashCredential::getBalance, oldBalance);
            if (credential.getExpireTime() != null && !credential.getExpireTime().after(now)) {
                updateWrapper.set(GiftCardCashCredential::getStatus, GiftCardCashCredentialStatusEnum.BOUND.name());
            }
            int updated = credentialMapper.update(null, updateWrapper);
            if (updated == 1) {
                actualRefundedToCard = actualRefundedToCard.add(newBalance.subtract(oldBalance)).setScale(2, RoundingMode.HALF_UP);
                saveGiftCardUsageRecord(orderSn, null, credential.getActivateMemberId(), credential,
                        newBalance.subtract(oldBalance), "INCREASE", bizType, orderSn, newBalance);
            } else {
                throw new ServiceException(ResultCode.ERROR, "礼品卡回退失败，请稍后重试");
            }
        }
        return actualRefundedToCard;
    }

    private List<OrderGiftCardRecord> listOrderGiftCardRecords(String orderSn) {
        return orderGiftCardRecordMapper.selectList(new LambdaQueryWrapper<OrderGiftCardRecord>()
                .eq(OrderGiftCardRecord::getOrderSn, orderSn)
                .eq(OrderGiftCardRecord::getDeleteFlag, false)
                .orderByAsc(OrderGiftCardRecord::getCreateTime)
                .orderByAsc(OrderGiftCardRecord::getId));
    }

    @Override
    public BigDecimal resolveChannelRefundAfterGiftCard(String orderSn, BigDecimal orderAmount) {
        BigDecimal channelAmount = toMoney(orderAmount);
        if (CharSequenceUtil.isBlank(orderSn)) {
            return zeroMoney();
        }
        refundOrderMemberCardBalance(orderSn, null, "ORDER_CANCEL");
        return channelAmount.compareTo(BigDecimal.ZERO) <= 0 ? zeroMoney() : channelAmount;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public BigDecimal resolveChannelRefundAfterGiftCardForAfterSale(AfterSale afterSale) {
        if (afterSale == null) {
            return zeroMoney();
        }
        BigDecimal totalAmount = toMoney(afterSale.getActualRefundPrice());
        if (totalAmount.compareTo(BigDecimal.ZERO) <= 0) {
            return zeroMoney();
        }
        BigDecimal giftCardRefundLimit = resolveGiftCardRefundLimit(afterSale);
        BigDecimal giftCardRefund = refundMemberCardBalance(afterSale, giftCardRefundLimit);
        BigDecimal channelRefund = totalAmount.subtract(giftCardRefund).setScale(2, RoundingMode.HALF_UP);
        return channelRefund.compareTo(BigDecimal.ZERO) <= 0 ? zeroMoney() : channelRefund;
    }

    @Override
    public void applyGiftCardUsageOnOrderCreate(TradeDTO tradeDTO, List<Order> orders, List<OrderItem> orderItems) {
        if (tradeDTO == null || CollUtil.isEmpty(orders)) {
            return;
        }
        Map<String, BigDecimal> orderDeductMap = applyGiftCardAllocation(orders, orderItems, tradeDTO);
        List<OrderGiftCardRecord> records = buildOrderGiftCardRecords(tradeDTO, orders, orderDeductMap);
        if (CollUtil.isNotEmpty(records)) {
            records.forEach(orderGiftCardRecordMapper::insert);
        }
        deductMemberCardBalance(tradeDTO.getMemberId(), tradeDTO.getGiftCardDeductItems());
    }

    /**
     * 执行导入发卡主流程：
     * 解析Excel -> 校验会员与库存 -> 创建发卡批次 -> 按行发卡。
     */
    private String runIssueImport(String activityId, MultipartFile file) {
        if (file == null || file.isEmpty()) {
            throw new ServiceException(ResultCode.FILE_NOT_EXIST_ERROR);
        }
        List<IssueRow> rows = parseIssueFile(file);
        if (rows.isEmpty()) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_ISSUE_IMPORT_ERROR);
        }
        if (rows.size() > MAX_ISSUE_ROWS) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_ISSUE_ROW_ERROR);
        }
        int totalNeed = rows.stream().mapToInt(IssueRow::quantity).sum();
        if (totalNeed > remainingActivityStock(activityId)) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_ISSUE_POOL_INSUFFICIENT);
        }
        Map<String, Member> mobileMember = new HashMap<>();
        for (IssueRow row : rows) {
            Member m = memberService.findByMobile(row.mobile);
            if (m == null || Boolean.TRUE.equals(m.getDeleteFlag())) {
                throw new ServiceException(ResultCode.GIFT_CARD_CASH_MEMBER_NOT_FOUND,
                        ResultCode.GIFT_CARD_CASH_MEMBER_NOT_FOUND.message() + "：" + row.mobile);
            }
            mobileMember.put(row.mobile, m);
        }
        String issueBatchId = SnowFlake.getIdStr();
        GiftCardCashIssueBatch issueBatch = new GiftCardCashIssueBatch();
        issueBatch.setId(issueBatchId);
        issueBatch.setActivityId(activityId);
        setIssueBatchOperator(issueBatch);
        issueBatch.setTotalCards(totalNeed);
        issueBatch.setTotalRows(rows.size());
        issueBatchMapper.insert(issueBatch);
        int cardSequence = 0;
        for (IssueRow row : rows) {
            Member member = mobileMember.get(row.mobile);
            cardSequence = issueNToMember(activityId, member.getId(), row.quantity, issueBatchId, cardSequence);
        }
        return issueBatchId;
    }

    /**
     * 给单个会员发放 N 张卡：直接生成凭证并绑定为待激活（不依赖制卡池），占用活动库存；会员激活后方可使用。
     *
     * @return 本批次已使用的最大卡序号
     */
    private int issueNToMember(String activityId, String memberId, int count, String issueBatchId, int startSequence) {
        if (remainingActivityStock(activityId) < count) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_ISSUE_POOL_INSUFFICIENT);
        }
        Date now = new Date();
        GiftCardCashActivity activity = getActivityOrThrow(activityId);
        BigDecimal face = activity.getFaceValue().setScale(2, RoundingMode.HALF_UP);
        int sequence = startSequence;
        for (int i = 0; i < count; i++) {
            sequence++;
            GiftCardCashCredential cred = new GiftCardCashCredential();
            cred.setId(SnowFlake.getIdStr());
            cred.setActivityId(activityId);
            cred.setIssueBatchId(issueBatchId);
            cred.setCardNo(buildSequentialCardNo(issueBatchId, sequence));
            cred.setCardSecret(RandomUtil.randomStringUpper(12));
            cred.setFaceValue(face);
            cred.setBalance(null);
            cred.setStatus(GiftCardCashCredentialStatusEnum.BOUND.name());
            cred.setMemberId(memberId);
            cred.setBindTime(now);
            cred.setActivateMemberId(null);
            cred.setActivateTime(null);
            cred.setExpireTime(null);
            credentialMapper.insert(cred);
        }
        return sequence;
    }

    private Map<String, BigDecimal> applyGiftCardAllocation(List<Order> orders, List<OrderItem> orderItems, TradeDTO tradeDTO) {
        Map<String, BigDecimal> orderDeductMap = new LinkedHashMap<>();
        BigDecimal totalDeduct = toMoney(tradeDTO.getGiftCardDeductTotal());
        BigDecimal totalCashPay = toMoney(tradeDTO.getCashPayTotal());
        if (totalDeduct.compareTo(BigDecimal.ZERO) <= 0 && totalCashPay.compareTo(BigDecimal.ZERO) <= 0) {
            return orderDeductMap;
        }
        BigDecimal orderAmountTotal = BigDecimal.ZERO;
        Map<String, BigDecimal> orderAmountMap = new HashMap<>();
        for (Order order : orders) {
            BigDecimal amount = amountBeforeGiftCard(order.getPriceDetailDTO(), order.getFlowPrice());
            orderAmountMap.put(order.getSn(), amount);
            orderAmountTotal = orderAmountTotal.add(amount);
        }
        if (orderAmountTotal.compareTo(BigDecimal.ZERO) <= 0) {
            return orderDeductMap;
        }
        BigDecimal deductedAssigned = BigDecimal.ZERO;
        BigDecimal cashAssigned = BigDecimal.ZERO;
        for (int i = 0; i < orders.size(); i++) {
            Order order = orders.get(i);
            BigDecimal orderAmount = orderAmountMap.get(order.getSn());
            BigDecimal orderDeduct;
            BigDecimal orderCash;
            if (i == orders.size() - 1) {
                orderDeduct = totalDeduct.subtract(deductedAssigned).setScale(2, RoundingMode.HALF_UP);
                orderCash = totalCashPay.subtract(cashAssigned).setScale(2, RoundingMode.HALF_UP);
            } else {
                BigDecimal ratio = orderAmount.divide(orderAmountTotal, 10, RoundingMode.HALF_UP);
                orderDeduct = totalDeduct.multiply(ratio).setScale(2, RoundingMode.HALF_UP);
                orderCash = totalCashPay.multiply(ratio).setScale(2, RoundingMode.HALF_UP);
                deductedAssigned = deductedAssigned.add(orderDeduct);
                cashAssigned = cashAssigned.add(orderCash);
            }
            PriceDetailDTO orderDetail = order.getPriceDetailDTO();
            if (orderDetail == null) {
                orderDetail = new PriceDetailDTO();
            }
            orderDetail.setGiftCardPrice(orderDeduct.doubleValue());
            order.setPriceDetailDTO(orderDetail);
            if (orderDeduct.compareTo(BigDecimal.ZERO) > 0) {
                order.setDeductionMethod("GIFT_CARD");
            }
            orderDeductMap.put(order.getSn(), orderDeduct);
            applyGiftCardAllocationToOrderItems(orderItems, order.getSn(), orderDeduct, orderCash, orderAmount);
        }
        return orderDeductMap;
    }

    private void applyGiftCardAllocationToOrderItems(List<OrderItem> orderItems, String orderSn, BigDecimal orderDeduct,
                                                     BigDecimal orderCash, BigDecimal orderAmount) {
        List<OrderItem> currentItems = orderItems.stream()
                .filter(item -> Objects.equals(orderSn, item.getOrderSn()))
                .collect(Collectors.toList());
        if (currentItems.isEmpty()) {
            return;
        }
        if (orderAmount.compareTo(BigDecimal.ZERO) <= 0) {
            for (OrderItem item : currentItems) {
                PriceDetailDTO detailDTO = item.getPriceDetailDTO();
                if (detailDTO == null) {
                    detailDTO = new PriceDetailDTO();
                }
                detailDTO.setGiftCardPrice(0D);
                item.setPriceDetailDTO(detailDTO);
            }
            return;
        }
        BigDecimal deductedAssigned = BigDecimal.ZERO;
        BigDecimal cashAssigned = BigDecimal.ZERO;
        for (int i = 0; i < currentItems.size(); i++) {
            OrderItem item = currentItems.get(i);
            BigDecimal itemAmount = amountBeforeGiftCard(item.getPriceDetailDTO(), item.getFlowPrice());
            BigDecimal itemDeduct;
            BigDecimal itemCash;
            if (i == currentItems.size() - 1) {
                itemDeduct = orderDeduct.subtract(deductedAssigned).setScale(2, RoundingMode.HALF_UP);
                itemCash = orderCash.subtract(cashAssigned).setScale(2, RoundingMode.HALF_UP);
            } else {
                BigDecimal ratio = itemAmount.divide(orderAmount, 10, RoundingMode.HALF_UP);
                itemDeduct = orderDeduct.multiply(ratio).setScale(2, RoundingMode.HALF_UP);
                itemCash = orderCash.multiply(ratio).setScale(2, RoundingMode.HALF_UP);
                deductedAssigned = deductedAssigned.add(itemDeduct);
                cashAssigned = cashAssigned.add(itemCash);
            }
            PriceDetailDTO detailDTO = item.getPriceDetailDTO();
            if (detailDTO == null) {
                detailDTO = new PriceDetailDTO();
            }
            detailDTO.setGiftCardPrice(itemDeduct.doubleValue());
            item.setPriceDetailDTO(detailDTO);
        }
    }

    /**
     * 礼品卡已经参与价格重算，flowPrice可能是抵扣后的现金应付。
     * 分摊使用记录时需要用“现金应付 + 礼品卡抵扣”还原本次可分摊金额。
     */
    private BigDecimal amountBeforeGiftCard(PriceDetailDTO detailDTO, Double fallbackFlowPrice) {
        BigDecimal flowPrice = toMoney(fallbackFlowPrice);
        if (detailDTO == null) {
            return flowPrice;
        }
        BigDecimal giftCardPrice = toMoney(detailDTO.getGiftCardPrice());
        return flowPrice.add(giftCardPrice).setScale(2, RoundingMode.HALF_UP);
    }

    private List<OrderGiftCardRecord> buildOrderGiftCardRecords(TradeDTO tradeDTO, List<Order> orders, Map<String, BigDecimal> orderDeductMap) {
        if (tradeDTO.getGiftCardDeductItems() == null || tradeDTO.getGiftCardDeductItems().isEmpty()) {
            return Collections.emptyList();
        }
        if (orders == null || orders.isEmpty()) {
            return Collections.emptyList();
        }
        List<OrderGiftCardRecord> records = new ArrayList<>();
        List<Order> deductibleOrders = orders.stream()
                .filter(order -> toMoney(orderDeductMap.get(order.getSn())).compareTo(BigDecimal.ZERO) > 0)
                .collect(Collectors.toList());
        if (deductibleOrders.isEmpty()) {
            return records;
        }
        BigDecimal totalOrderDeduct = deductibleOrders.stream()
                .map(order -> toMoney(orderDeductMap.get(order.getSn())))
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        if (totalOrderDeduct.compareTo(BigDecimal.ZERO) <= 0) {
            return records;
        }
        for (GiftCardCashDeductPreviewVO.DeductItem item : tradeDTO.getGiftCardDeductItems()) {
            if (item == null || item.getDeductAmount() == null || item.getDeductAmount().compareTo(BigDecimal.ZERO) <= 0) {
                continue;
            }
            GiftCardCashCredential credential = credentialMapper.selectById(item.getCredentialId());
            if (credential == null || Boolean.TRUE.equals(credential.getDeleteFlag())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡不存在或已失效");
            }
            BigDecimal runningBalance = credential.getBalance() == null
                    ? zeroMoney() : credential.getBalance().setScale(2, RoundingMode.HALF_UP);
            BigDecimal cardDeductTotal = item.getDeductAmount().setScale(2, RoundingMode.HALF_UP);
            BigDecimal assigned = BigDecimal.ZERO;
            for (int i = 0; i < deductibleOrders.size(); i++) {
                Order order = deductibleOrders.get(i);
                BigDecimal orderDeduct = toMoney(orderDeductMap.get(order.getSn()));
                BigDecimal cardOrderDeduct;
                if (i == deductibleOrders.size() - 1) {
                    cardOrderDeduct = cardDeductTotal.subtract(assigned).setScale(2, RoundingMode.HALF_UP);
                } else {
                    BigDecimal ratio = orderDeduct.divide(totalOrderDeduct, 10, RoundingMode.HALF_UP);
                    cardOrderDeduct = cardDeductTotal.multiply(ratio).setScale(2, RoundingMode.HALF_UP);
                    assigned = assigned.add(cardOrderDeduct);
                }
                if (cardOrderDeduct.compareTo(BigDecimal.ZERO) <= 0) {
                    continue;
                }
                OrderGiftCardRecord record = new OrderGiftCardRecord();
                record.setId(SnowFlake.getIdStr());
                record.setOrderSn(order.getSn());
                record.setTradeSn(tradeDTO.getSn());
                record.setMemberId(tradeDTO.getMemberId());
                record.setCredentialId(item.getCredentialId());
                record.setCardNo(item.getCardNo());
                record.setDeductAmount(cardOrderDeduct);
                record.setChangeAmount(cardOrderDeduct.negate());
                runningBalance = runningBalance.subtract(cardOrderDeduct).setScale(2, RoundingMode.HALF_UP);
                record.setBalanceAfter(runningBalance);
                record.setFlowType("DECREASE");
                record.setBizType("ORDER_DEDUCT");
                record.setBizSn(order.getSn());
                records.add(record);
            }
        }
        return records;
    }

    private BigDecimal toMoney(BigDecimal value) {
        return value == null ? zeroMoney() : value.setScale(2, RoundingMode.HALF_UP);
    }

    private BigDecimal toMoney(Double value) {
        return value == null ? zeroMoney() : BigDecimal.valueOf(value).setScale(2, RoundingMode.HALF_UP);
    }

    private BigDecimal zeroMoney() {
        return BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
    }

    private String formatDateTimeForSql(Date date) {
        return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date);
    }

    /**
     * 根据售后商品分摊的礼品卡优惠金额，计算本次允许回卡上限。
     */
    private BigDecimal resolveGiftCardRefundLimit(AfterSale afterSale) {
        if (afterSale == null || afterSale.getActualRefundPrice() == null) {
            return zeroMoney();
        }
        double actualRefund = afterSale.getActualRefundPrice();
        if (actualRefund <= 0D) {
            return zeroMoney();
        }
        OrderItem orderItem = orderItemService.getBySn(afterSale.getOrderItemSn());
        if (orderItem == null || orderItem.getNum() == null || orderItem.getNum() <= 0) {
            return toMoney(actualRefund);
        }
        PriceDetailDTO detail = orderItem.getPriceDetailDTO();
        double itemGiftCard = detail == null || detail.getGiftCardPrice() == null ? 0D : detail.getGiftCardPrice();
        if (itemGiftCard <= 0D) {
            return zeroMoney();
        }
        double unitGiftCard = itemGiftCard / orderItem.getNum();
        double refundItemGiftCard = unitGiftCard * afterSale.getNum();
        BigDecimal upper = toMoney(refundItemGiftCard);
        BigDecimal actual = toMoney(actualRefund);
        if (upper.compareTo(BigDecimal.ZERO) < 0) {
            upper = zeroMoney();
        }
        return upper.min(actual);
    }

    /**
     * 写入礼品卡使用记录（增减通用）。
     */
    private void saveGiftCardUsageRecord(String orderSn, String tradeSn, String memberId, GiftCardCashCredential credential,
                                         BigDecimal changeAmount, String flowType, String bizType, String bizSn, BigDecimal balanceAfter) {
        if (credential == null || changeAmount == null || changeAmount.compareTo(BigDecimal.ZERO) == 0) {
            return;
        }
        OrderGiftCardRecord record = new OrderGiftCardRecord();
        record.setId(SnowFlake.getIdStr());
        record.setOrderSn(orderSn);
        record.setTradeSn(tradeSn);
        record.setMemberId(memberId);
        record.setCredentialId(credential.getId());
        record.setCardNo(credential.getCardNo());
        if (changeAmount.compareTo(BigDecimal.ZERO) < 0) {
            record.setDeductAmount(changeAmount.abs().setScale(2, RoundingMode.HALF_UP));
        } else {
            record.setDeductAmount(zeroMoney());
        }
        record.setChangeAmount(changeAmount.setScale(2, RoundingMode.HALF_UP));
        record.setBalanceAfter(balanceAfter == null ? null : balanceAfter.setScale(2, RoundingMode.HALF_UP));
        record.setFlowType(flowType);
        record.setBizType(bizType);
        record.setBizSn(bizSn);
        orderGiftCardRecordMapper.insert(record);
    }

    /**
     * 解析发卡导入文件。
     * 从第2行起读取：第1列手机号，第2列发卡数量。
     */
    private List<IssueRow> parseIssueFile(MultipartFile file) {
        try (InputStream inputStream = file.getInputStream()) {
            ExcelReader reader = ExcelUtil.getReader(inputStream);
            int last = reader.getRowCount();
            if (last < 2) {
                return Collections.emptyList();
            }
            List<List<Object>> data = reader.read(1, last);
            List<IssueRow> parsed = new ArrayList<>();
            for (List<Object> objects : data) {
                if (objects == null || objects.isEmpty()) {
                    continue;
                }
                Object c0 = objects.get(0);
                if (c0 == null || CharSequenceUtil.isBlank(Convert.toStr(c0))) {
                    continue;
                }
                String mobile = normalizeMobile(c0);
                if (objects.size() < 2 || objects.get(1) == null) {
                    throw new ServiceException(ResultCode.GIFT_CARD_CASH_ISSUE_ROW_ERROR);
                }
                int qty = Convert.toInt(objects.get(1), 0);
                if (qty < 1 || qty > 10000) {
                    throw new ServiceException(ResultCode.GIFT_CARD_CASH_ISSUE_ROW_ERROR);
                }
                parsed.add(new IssueRow(mobile, qty));
            }
            return parsed;
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            log.error("parseIssueFile", e);
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_ISSUE_IMPORT_ERROR);
        }
    }

    /** 标准化手机号单元格文本，去掉Excel数字格式带出的“.0”尾缀。 */
    private static String normalizeMobile(Object cell) {
        String s = Convert.toStr(cell, "").trim();
        if (s.endsWith(".0")) {
            s = s.substring(0, s.length() - 2);
        }
        return s;
    }

    /** 会员礼品卡VO转换。 */
    private GiftCardCashMemberCardVO toMemberCardVo(GiftCardCashCredential credential) {
        GiftCardCashMemberCardVO vo = new GiftCardCashMemberCardVO();
        BeanUtils.copyProperties(credential, vo);
        GiftCardCashActivity activity = activityMapper.selectById(credential.getActivityId());
        if (activity != null && !Boolean.TRUE.equals(activity.getDeleteFlag())) {
            vo.setGiftCardName(activity.getGiftCardName());
        }
        vo.setMemberCardStatus(resolveMemberCardStatus(credential, new Date()));
        return vo;
    }

    /** 根据凭证信息计算买家端展示状态。 */
    private String resolveMemberCardStatus(GiftCardCashCredential credential, Date now) {
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
        boolean hasBalance = credential.getBalance() != null && credential.getBalance().compareTo(BigDecimal.ZERO) > 0;
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
    private Date calculateExpireTime(GiftCardCashActivity activity, Date bindTime) {
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

    /**
     * 生成18位顺序卡号：C + 10位批次短码 + 6位递增序号（000001开始）。
     */
    private String buildSequentialCardNo(String batchId, int sequence) {
        String batchCode = buildBatchCardCode(batchId);
        return "C" + batchCode + String.format("%06d", sequence);
    }

    private String buildBatchCardCode(String batchId) {
        String source = CharSequenceUtil.blankToDefault(batchId, "0").replaceAll("\\D", "");
        if (CharSequenceUtil.isBlank(source)) {
            source = String.valueOf(Integer.toUnsignedLong(batchId.hashCode()));
        }
        if (source.length() > 10) {
            source = source.substring(source.length() - 10);
        }
        return String.format("%10s", source).replace(' ', '0');
    }

    /** 写入制卡批次操作人信息（姓名、手机号）。 */
    private void setCreateBatchOperator(GiftCardCashCreateBatch batch) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            return;
        }
        String createUserName = CharSequenceUtil.blankToDefault(
                CharSequenceUtil.trim(currentUser.getNickName()), currentUser.getUsername());
        batch.setCreateUserName(createUserName);
        String mobile = null;
        if (CharSequenceUtil.isNotBlank(currentUser.getId())) {
            AdminUser adminUser = adminUserService.getById(currentUser.getId());
            if (adminUser != null) {
                mobile = CharSequenceUtil.blankToDefault(CharSequenceUtil.trim(adminUser.getMobile()), null);
            }
        }
        batch.setCreateUserMobile(mobile);
    }

    /** 写入发卡批次操作人信息（姓名、手机号）。 */
    private void setIssueBatchOperator(GiftCardCashIssueBatch issueBatch) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            return;
        }
        String issueUserName = CharSequenceUtil.blankToDefault(
                CharSequenceUtil.trim(currentUser.getNickName()), currentUser.getUsername());
        issueBatch.setIssueUserName(issueUserName);
        String mobile = null;
        if (CharSequenceUtil.isNotBlank(currentUser.getId())) {
            AdminUser adminUser = adminUserService.getById(currentUser.getId());
            if (adminUser != null) {
                mobile = CharSequenceUtil.blankToDefault(CharSequenceUtil.trim(adminUser.getMobile()), null);
            }
        }
        issueBatch.setIssueUserMobile(mobile);
    }

    private record IssueRow(String mobile, int quantity) {
    }


    /** 根据活动ID查询活动，若不存在或已逻辑删除则抛业务异常。 */
    private GiftCardCashActivity getActivityOrThrow(String id) {
        GiftCardCashActivity a = activityMapper.selectById(id);
        if (a == null || Boolean.TRUE.equals(a.getDeleteFlag())) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_ACTIVITY_NOT_FOUND);
        }
        return a;
    }

    /** 根据制卡批次ID查询批次，若不存在或已逻辑删除则抛业务异常。 */
    private GiftCardCashCreateBatch getCreateBatchOrThrow(String id) {
        GiftCardCashCreateBatch b = createBatchMapper.selectById(id);
        if (b == null || Boolean.TRUE.equals(b.getDeleteFlag())) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_CREATE_BATCH_NOT_FOUND);
        }
        return b;
    }
}
