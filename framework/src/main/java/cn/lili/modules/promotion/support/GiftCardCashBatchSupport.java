package cn.lili.modules.promotion.support;

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
import cn.lili.modules.permission.entity.dos.AdminUser;
import cn.lili.modules.permission.service.AdminUserService;
import cn.lili.modules.promotion.entity.dos.GiftCardCashActivity;
import cn.lili.modules.promotion.entity.dos.GiftCardCashCreateBatch;
import cn.lili.modules.promotion.entity.dos.GiftCardCashCredential;
import cn.lili.modules.promotion.entity.dos.GiftCardCashIssueBatch;
import cn.lili.modules.promotion.entity.dto.GiftCardCashBatchCreateDTO;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashCreateBatchSearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashIssueBatchSearchParams;
import cn.lili.modules.promotion.entity.enums.GiftCardCashCredentialStatusEnum;
import cn.lili.modules.promotion.entity.vos.GiftCardCashCreateBatchVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashIssueBatchVO;
import cn.lili.modules.promotion.mapper.GiftCardCashActivityMapper;
import cn.lili.modules.promotion.mapper.GiftCardCashCreateBatchMapper;
import cn.lili.modules.promotion.mapper.GiftCardCashCredentialMapper;
import cn.lili.modules.promotion.mapper.GiftCardCashIssueBatchMapper;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.multipart.MultipartFile;

import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * 现金/提货礼品卡制卡批次、发卡批次、卡密导出与批量导入发卡支持类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
public class GiftCardCashBatchSupport {

    /** 单次导入发卡允许的最大数据行数（不含表头）。 */
    private static final int MAX_ISSUE_ROWS = 1000;

    /**
     * 批量制卡：校验库存后创建制卡批次，并生成对应数量的卡密凭证。
     *
     * @param activityMapper 活动持久层映射
     * @param createBatchMapper 制卡批次映射
     * @param credentialMapper 卡密凭证映射
     * @param adminUserService 管理员服务
     * @param activityId 活动ID
     * @param dto 制卡参数
     * @return 新生成的制卡批次ID
     */
    public static String batchCreateCards(GiftCardCashActivityMapper activityMapper,
                                         GiftCardCashCreateBatchMapper createBatchMapper,
                                         GiftCardCashCredentialMapper credentialMapper,
                                         AdminUserService adminUserService,
                                         String activityId,
                                         GiftCardCashBatchCreateDTO dto) {
        int quantity = dto.getQuantity();
        if (quantity < 1 || quantity > 10000) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_CREATE_QUANTITY_ERROR);
        }
        GiftCardCashActivity activity = lockActivity(activityMapper, activityId);
        long produced = countCredentialsByActivity(credentialMapper, activityId);
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
        setCreateBatchOperator(adminUserService, batch);
        createBatchMapper.insert(batch);
        for (int i = 0; i < quantity; i++) {
            GiftCardCashCredential cred = new GiftCardCashCredential();
            cred.setId(SnowFlake.getIdStr());
            cred.setActivityId(activityId);
            cred.setCreateBatchId(batchId);
            // 卡号按批次内递增生成：保证可读顺序；batchId 唯一保证跨批次不重复
            cred.setCardNo(buildSequentialCardNo(batchId, i + 1));
            cred.setCardSecret(RandomUtil.randomStringUpper(12));
            cred.setCardType(activity.getCardType());
            cred.setFaceValue(face);
            cred.setBalance(face);
            cred.setStatus(GiftCardCashCredentialStatusEnum.UNISSUED.name());
            credentialMapper.insert(cred);
        }
        return batchId;
    }

    /**
     * 分页查询制卡批次。
     *
     * @param createBatchMapper 制卡批次映射
     * @param params 查询条件
     * @param page 分页参数
     * @return 制卡批次分页对象
     */
    public static IPage<GiftCardCashCreateBatchVO> pageCreateBatch(GiftCardCashCreateBatchMapper createBatchMapper,
                                                                   GiftCardCashCreateBatchSearchParams params,
                                                                   PageVO page) {
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
            applyCredentialExists(w, "c.create_batch_id = cb.id",
                    params.getCardNo(), params.getExpireTimeStart(), params.getExpireTimeEnd());
        }
        w.orderByDesc("cb.create_time");
        return createBatchMapper.pageByParams(mpPage, w);
    }

    /**
     * 分页查询发卡批次记录。
     *
     * @param issueBatchMapper 发卡批次映射
     * @param params 查询条件
     * @param page 分页参数
     * @return 发卡批次分页对象
     */
    public static IPage<GiftCardCashIssueBatchVO> pageIssueBatch(GiftCardCashIssueBatchMapper issueBatchMapper,
                                                                 GiftCardCashIssueBatchSearchParams params,
                                                                 PageVO page) {
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
            applyCredentialExists(w, "c.issue_batch_id = ib.id",
                    params.getCardNo(), params.getExpireTimeStart(), params.getExpireTimeEnd());
        }
        w.orderByDesc("ib.create_time");
        return issueBatchMapper.pageByParams(mpPage, w);
    }

    /**
     * 导出制卡批次下全部卡密（卡号、卡密、面值、礼品卡名称等）。
     *
     * @param createBatchMapper 制卡批次映射
     * @param credentialMapper 卡密映射
     * @param activityMapper 活动映射
     * @param createBatchId 制卡批次ID
     * @param response HTTP响应对象
     */
    public static void exportCreateBatch(GiftCardCashCreateBatchMapper createBatchMapper,
                                         GiftCardCashCredentialMapper credentialMapper,
                                         GiftCardCashActivityMapper activityMapper,
                                         String createBatchId,
                                         HttpServletResponse response) {
        GiftCardCashCreateBatch batch = getCreateBatchOrThrow(createBatchMapper, createBatchId);
        List<GiftCardCashCredential> list = credentialMapper.selectList(new LambdaQueryWrapper<GiftCardCashCredential>()
                .eq(GiftCardCashCredential::getCreateBatchId, createBatchId)
                .eq(GiftCardCashCredential::getDeleteFlag, false)
                .orderByAsc(GiftCardCashCredential::getCardNo));
        GiftCardCashActivity activity = getActivityOrThrow(activityMapper, batch.getActivityId());
        String batchLabel = CharSequenceUtil.blankToDefault(activity.getGiftCardName(), "-");
        ExcelWriter writer = ExcelUtil.getWriter(true);
        writer.writeHeadRow(CollUtil.newArrayList("卡号", "卡密", "面值", "礼品卡名称", "制卡批次ID", "卡类型", "兑换方式", "兑换种数"));
        for (GiftCardCashCredential cred : list) {
            writer.writeRow(CollUtil.newArrayList(
                    cred.getCardNo(),
                    cred.getCardSecret(),
                    cred.getFaceValue().toPlainString(),
                    batchLabel,
                    createBatchId, activity.getCardType(), activity.getPickupMode(), activity.getPickupCount()
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

    /**
     * 下载批量发卡导入模板（手机号、发卡数量）。
     *
     * @param response HTTP响应对象
     */
    public static void downloadIssueTemplate(HttpServletResponse response) {
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

    /**
     * 按活动导入发卡：直接生成凭证并发放给会员，扣减活动库存（不依赖制卡池）。
     *
     * @param activityMapper 活动映射
     * @param issueBatchMapper 发卡批次映射
     * @param credentialMapper 卡密映射
     * @param memberService 会员服务
     * @param adminUserService 管理员服务
     * @param activityId 活动ID
     * @param file 包含手机号和发卡数量的Excel文件
     * @return 发卡批次ID
     */
    public static String issueImportByActivity(GiftCardCashActivityMapper activityMapper,
                                               GiftCardCashIssueBatchMapper issueBatchMapper,
                                               GiftCardCashCredentialMapper credentialMapper,
                                               MemberService memberService,
                                               AdminUserService adminUserService,
                                               String activityId,
                                               MultipartFile file) {
        getActivityOrThrow(activityMapper, activityId);
        return runIssueImport(activityMapper, issueBatchMapper, credentialMapper, memberService, adminUserService, activityId, file);
    }

    /**
     * 为批次分页查询按卡密凭证条件拼接参数化 EXISTS 子查询，避免字符串直接拼接引入 SQL 注入隐患。
     *
     * @param wrapper 批次查询包装器
     * @param joinCondition 关联字段条件（例如 "c.create_batch_id = cb.id" 或 "c.issue_batch_id = ib.id"）
     * @param cardNo 卡号（支持模糊匹配）
     * @param expireTimeStart 过期时间起始
     * @param expireTimeEnd 过期时间截止
     */
    public static <T> void applyCredentialExists(QueryWrapper<T> wrapper, String joinCondition,
                                                 String cardNo, Date expireTimeStart, Date expireTimeEnd) {
        boolean needCredentialExists = CharSequenceUtil.isNotBlank(cardNo)
                || expireTimeStart != null
                || expireTimeEnd != null;
        if (!needCredentialExists) {
            return;
        }
        List<Object> sqlParams = new ArrayList<>();
        StringBuilder existsSql = new StringBuilder();
        existsSql.append("EXISTS (SELECT 1 FROM li_gift_card_cash_credential c ")
                .append("WHERE c.delete_flag = 0 AND ")
                .append(joinCondition);
        if (CharSequenceUtil.isNotBlank(cardNo)) {
            existsSql.append(" AND c.card_no LIKE {").append(sqlParams.size()).append("}");
            sqlParams.add("%" + cardNo.trim() + "%");
        }
        if (expireTimeStart != null) {
            existsSql.append(" AND c.expire_time >= {").append(sqlParams.size()).append("}");
            sqlParams.add(expireTimeStart);
        }
        if (expireTimeEnd != null) {
            existsSql.append(" AND c.expire_time <= {").append(sqlParams.size()).append("}");
            sqlParams.add(expireTimeEnd);
        }
        existsSql.append(")");
        wrapper.apply(existsSql.toString(), sqlParams.toArray());
    }

    /**
     * 执行导入发卡主流程：
     * 解析Excel -> 校验会员与库存 -> 创建发卡批次 -> 按行发卡。
     */
    private static String runIssueImport(GiftCardCashActivityMapper activityMapper,
                                         GiftCardCashIssueBatchMapper issueBatchMapper,
                                         GiftCardCashCredentialMapper credentialMapper,
                                         MemberService memberService,
                                         AdminUserService adminUserService,
                                         String activityId,
                                         MultipartFile file) {
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
        lockActivity(activityMapper, activityId);
        int totalNeed = rows.stream().mapToInt(IssueRow::quantity).sum();
        if (totalNeed > remainingActivityStock(activityMapper, credentialMapper, activityId)) {
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
        setIssueBatchOperator(adminUserService, issueBatch);
        issueBatch.setTotalCards(totalNeed);
        issueBatch.setTotalRows(rows.size());
        issueBatchMapper.insert(issueBatch);
        int cardSequence = 0;
        for (IssueRow row : rows) {
            Member member = mobileMember.get(row.mobile);
            cardSequence = issueNToMember(activityMapper, credentialMapper, activityId, member.getId(), row.quantity, issueBatchId, cardSequence);
        }
        return issueBatchId;
    }

    /**
     * 给单个会员发放 N 张卡：直接生成凭证并绑定为待激活（不依赖制卡池），占用活动库存；会员激活后方可使用。
     *
     * @return 本批次已使用的最大卡序号
     */
    private static int issueNToMember(GiftCardCashActivityMapper activityMapper,
                                      GiftCardCashCredentialMapper credentialMapper,
                                      String activityId,
                                      String memberId,
                                      int count,
                                      String issueBatchId,
                                      int startSequence) {
        if (remainingActivityStock(activityMapper, credentialMapper, activityId) < count) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_ISSUE_POOL_INSUFFICIENT);
        }
        Date now = new Date();
        GiftCardCashActivity activity = lockActivity(activityMapper, activityId);
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
            cred.setCardType(activity.getCardType());
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

    /**
     * 解析发卡导入文件。
     * 从第2行起读取：第1列手机号，第2列发卡数量。
     */
    private static List<IssueRow> parseIssueFile(MultipartFile file) {
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

    /**
     * 生成18位顺序卡号：C + 10位批次短码 + 6位递增序号（000001开始）。
     */
    public static String buildSequentialCardNo(String batchId, int sequence) {
        String batchCode = buildBatchCardCode(batchId);
        return "C" + batchCode + String.format("%06d", sequence);
    }

    public static String buildBatchCardCode(String batchId) {
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
    private static void setCreateBatchOperator(AdminUserService adminUserService, GiftCardCashCreateBatch batch) {
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
    private static void setIssueBatchOperator(AdminUserService adminUserService, GiftCardCashIssueBatch issueBatch) {
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

    /** 根据活动ID查询活动，若不存在或已逻辑删除则抛业务异常（加锁）。 */
    public static GiftCardCashActivity lockActivity(GiftCardCashActivityMapper activityMapper, String id) {
        GiftCardCashActivity activity = activityMapper.selectOne(new LambdaQueryWrapper<GiftCardCashActivity>()
                .eq(GiftCardCashActivity::getId, id).eq(GiftCardCashActivity::getDeleteFlag, false).last("FOR UPDATE"));
        if (activity == null) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_ACTIVITY_NOT_FOUND);
        }
        return activity;
    }

    /** 根据活动ID查询活动，若不存在或已逻辑删除则抛业务异常。 */
    public static GiftCardCashActivity getActivityOrThrow(GiftCardCashActivityMapper activityMapper, String id) {
        GiftCardCashActivity a = activityMapper.selectById(id);
        if (a == null || Boolean.TRUE.equals(a.getDeleteFlag())) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_ACTIVITY_NOT_FOUND);
        }
        return a;
    }

    /** 根据制卡批次ID查询批次，若不存在或已逻辑删除则抛业务异常。 */
    public static GiftCardCashCreateBatch getCreateBatchOrThrow(GiftCardCashCreateBatchMapper createBatchMapper, String id) {
        GiftCardCashCreateBatch b = createBatchMapper.selectById(id);
        if (b == null || Boolean.TRUE.equals(b.getDeleteFlag())) {
            throw new ServiceException(ResultCode.GIFT_CARD_CASH_CREATE_BATCH_NOT_FOUND);
        }
        return b;
    }

    /** 统计活动下总卡密数量（含已发、未发，不含逻辑删除）。 */
    public static long countCredentialsByActivity(GiftCardCashCredentialMapper credentialMapper, String activityId) {
        return credentialMapper.selectCount(new LambdaQueryWrapper<GiftCardCashCredential>()
                .eq(GiftCardCashCredential::getActivityId, activityId)
                .eq(GiftCardCashCredential::getDeleteFlag, false));
    }

    /** 活动剩余可占用库存（制卡、发卡共用）。 */
    public static int remainingActivityStock(GiftCardCashActivityMapper activityMapper,
                                             GiftCardCashCredentialMapper credentialMapper,
                                             String activityId) {
        GiftCardCashActivity activity = lockActivity(activityMapper, activityId);
        int stockTotal = activity.getStockTotal() == null ? 0 : activity.getStockTotal();
        return stockTotal - (int) countCredentialsByActivity(credentialMapper, activityId);
    }

    @Schema(description = "导入发卡单行数据载体")
    private record IssueRow(
            @Schema(description = "会员手机号")
            String mobile,
            @Schema(description = "发放数量")
            int quantity
    ) {
    }
}
