package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.poi.excel.ExcelReader;
import cn.hutool.poi.excel.ExcelUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.utils.UuidUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.enums.SourceEnum;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.entity.dos.DistributionImportBatch;
import cn.lili.modules.distribution.entity.dos.DistributionImportItem;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.mapper.DistributionImportBatchMapper;
import cn.lili.modules.distribution.mapper.DistributionImportItemMapper;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.service.DistributionMemberImportService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.support.DistributionMemberSupport;
import cn.lili.modules.finance.export.FinanceExportHelper;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.MemberAddDTO;
import cn.lili.modules.member.service.MemberService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * 分销员批量导入实现
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Slf4j
@Service
public class DistributionMemberImportServiceImpl
        extends ServiceImpl<DistributionImportBatchMapper, DistributionImportBatch>
        implements DistributionMemberImportService {

    private static final Pattern MOBILE_PATTERN = Pattern.compile("^1[3-9]\\d{9}$");
    private static final int MAX_IMPORT_ROWS = 5000;
    private static final List<String> TEMPLATE_HEADERS = List.of(
            "手机号", "分销员openid", "昵称", "邀请人手机号", "邀请人openid", "等级序号");

    @Autowired
    private DistributionImportItemMapper importItemMapper;

    @Autowired
    private MemberService memberService;

    @Autowired
    private ConnectService connectService;

    @Autowired
    private DistributionMemberSupport distributionMemberSupport;

    @Autowired
    private DistributionGradeService gradeService;

    @Autowired
    private DistributionService distributionService;

    @Autowired
    private DistributionCustomerRelationService customerRelationService;

    /**
     * 下载分销员批量导入 Excel 模板。
     *
     * @param response HTTP 响应
     */
    @Override
    public void downloadTemplate(HttpServletResponse response) {
        List<List<Object>> sample = List.of(List.of(
                "13800138000", "", "示例分销员", "", "", "1"));
        FinanceExportHelper.writeExcel(response, "分销员批量导入模板", writer ->
                FinanceExportHelper.writeSheet(writer, "分销员导入", TEMPLATE_HEADERS, sample));
    }

    /**
     * 批量导入分销员 Excel。
     *
     * @param file 导入文件
     * @return 导入批次结果
     */
    @Override
    public DistributionImportBatch importExcel(MultipartFile file) {
        if (file == null || file.isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请上传导入文件");
        }
        DistributionImportBatch batch = new DistributionImportBatch();
        batch.setFileName(file.getOriginalFilename());
        batch.setTotalCount(0);
        batch.setSuccessCount(0);
        batch.setFailCount(0);
        batch.setStatus("PROCESSING");
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser != null) {
            batch.setOperatorId(currentUser.getId());
            batch.setOperatorName(StringUtils.isNotEmpty(currentUser.getNickName())
                    ? currentUser.getNickName() : currentUser.getUsername());
        }
        this.save(batch);

        // 跳过表头行（从第 2 行开始读取）
        List<List<Object>> rows;
        try (InputStream inputStream = file.getInputStream()) {
            ExcelReader reader = ExcelUtil.getReader(inputStream);
            rows = reader.read(1);
        } catch (Exception e) {
            batch.setStatus("FAILED");
            batch.setRemark("读取Excel失败：" + e.getMessage());
            this.updateById(batch);
            throw new ServiceException(ResultCode.PARAMS_ERROR, "读取Excel失败");
        }

        if (rows == null) {
            rows = new ArrayList<>();
        }
        if (rows.size() > MAX_IMPORT_ROWS) {
            batch.setStatus("FAILED");
            batch.setRemark("单次最多导入" + MAX_IMPORT_ROWS + "行");
            this.updateById(batch);
            throw new ServiceException(ResultCode.PARAMS_ERROR, "单次最多导入" + MAX_IMPORT_ROWS + "行");
        }

        int success = 0;
        int fail = 0;
        int rowNum = 1;
        // 逐行导入，单行失败不影响其他行
        for (List<Object> row : rows) {
            rowNum++;
            if (isBlankRow(row)) {
                continue;
            }
            DistributionImportItem item = new DistributionImportItem();
            item.setBatchId(batch.getId());
            item.setRowNum(rowNum);
            try {
                fillItemFromRow(item, row);
                String distributionId = importOneRow(item);
                item.setDistributionId(distributionId);
                item.setStatus("SUCCESS");
                success++;
            } catch (ServiceException ex) {
                item.setStatus("FAIL");
                item.setFailReason(resolveFailReason(ex));
                fail++;
            } catch (Exception ex) {
                log.warn("分销员导入第{}行失败", rowNum, ex);
                item.setStatus("FAIL");
                item.setFailReason(ex.getMessage() == null ? "导入失败" : ex.getMessage());
                fail++;
            }
            importItemMapper.insert(item);
        }

        batch.setTotalCount(success + fail);
        batch.setSuccessCount(success);
        batch.setFailCount(fail);
        batch.setStatus("DONE");
        this.updateById(batch);
        return batch;
    }

    /**
     * 分页查询导入批次。
     *
     * @param page 分页参数
     * @return 批次分页
     */
    @Override
    public IPage<DistributionImportBatch> pageBatches(PageVO page) {
        return this.page(PageUtil.initPage(page),
                new LambdaQueryWrapper<DistributionImportBatch>().orderByDesc(DistributionImportBatch::getCreateTime));
    }

    /**
     * 分页查询指定批次的导入明细。
     *
     * @param batchId 批次ID
     * @param status  明细状态 SUCCESS/FAIL
     * @param page    分页参数
     * @return 明细分页
     */
    @Override
    public IPage<DistributionImportItem> pageItems(String batchId, String status, PageVO page) {
        if (StringUtils.isEmpty(batchId)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "批次ID不能为空");
        }
        LambdaQueryWrapper<DistributionImportItem> wrapper = new LambdaQueryWrapper<DistributionImportItem>()
                .eq(DistributionImportItem::getBatchId, batchId)
                .eq(StringUtils.isNotEmpty(status), DistributionImportItem::getStatus, status)
                .orderByAsc(DistributionImportItem::getRowNum);
        return importItemMapper.selectPage(PageUtil.initPage(page), wrapper);
    }

    /**
     * 导出指定批次的失败明细 Excel。
     *
     * @param batchId  批次ID
     * @param response HTTP 响应
     */
    @Override
    public void downloadFailItems(String batchId, HttpServletResponse response) {
        if (StringUtils.isEmpty(batchId)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "批次ID不能为空");
        }
        DistributionImportBatch batch = this.getById(batchId);
        if (batch == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "导入记录不存在");
        }
        List<DistributionImportItem> failItems = importItemMapper.selectList(
                new LambdaQueryWrapper<DistributionImportItem>()
                        .eq(DistributionImportItem::getBatchId, batchId)
                        .eq(DistributionImportItem::getStatus, "FAIL")
                        .orderByAsc(DistributionImportItem::getRowNum));
        if (failItems == null || failItems.isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "该批次没有失败记录");
        }
        List<String> headers = List.of(
                "行号", "手机号", "分销员openid", "昵称", "邀请人手机号", "邀请人openid", "等级序号", "失败原因");
        List<List<Object>> rows = new ArrayList<>();
        for (DistributionImportItem item : failItems) {
            rows.add(List.of(
                    item.getRowNum() == null ? "" : item.getRowNum(),
                    Objects.toString(item.getMobile(), ""),
                    Objects.toString(item.getOpenId(), ""),
                    Objects.toString(item.getNickname(), ""),
                    Objects.toString(item.getInviterMobile(), ""),
                    Objects.toString(item.getInviterOpenId(), ""),
                    item.getGradeSort() == null ? "" : item.getGradeSort(),
                    Objects.toString(item.getFailReason(), "")
            ));
        }
        FinanceExportHelper.writeExcel(response, "分销员导入失败列表", writer ->
                FinanceExportHelper.writeSheet(writer, "失败列表", headers, rows));
    }

    /**
     * 导入单行分销员数据（创建会员、分销员及邀请关系）。
     *
     * @param item 导入明细
     * @return 分销员ID
     */
    @Transactional(rollbackFor = Exception.class)
    public String importOneRow(DistributionImportItem item) {
        String mobile = trim(item.getMobile());
        String nickname = trim(item.getNickname());
        String openId = trim(item.getOpenId());
        String inviterMobile = trim(item.getInviterMobile());
        String inviterOpenId = trim(item.getInviterOpenId());
        Integer gradeSort = item.getGradeSort();

        if (StringUtils.isEmpty(mobile)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "手机号不能为空");
        }
        if (!MOBILE_PATTERN.matcher(mobile).matches()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "手机号格式有误");
        }
        if (StringUtils.isEmpty(nickname)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "昵称不能为空");
        }
        if (gradeSort == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "等级序号不能为空");
        }
        DistributionGrade grade = gradeService.resolveGrade(String.valueOf(gradeSort));
        if (grade == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "等级序号不存在：" + gradeSort);
        }
        if (StringUtils.isNotEmpty(inviterMobile) && !MOBILE_PATTERN.matcher(inviterMobile).matches()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人手机号格式有误");
        }

        Member member = memberService.findByMobile(mobile);
        // 会员不存在则自动注册，随机密码
        if (member == null) {
            MemberAddDTO memberAddDTO = new MemberAddDTO();
            memberAddDTO.setUsername(mobile);
            memberAddDTO.setMobile(mobile);
            memberAddDTO.setPassword(UuidUtils.getUUID());
            member = memberService.addMember(memberAddDTO);
        }
        member.setNickName(nickname);
        memberService.assignDefaultMemberGradeIfAbsent(member);
        memberService.updateById(member);

        bindOpenId(member.getId(), openId);

        // 按分销员状态决定新建、激活或仅更新昵称
        Distribution distribution = distributionMemberSupport.getByMemberId(member.getId());
        if (distribution == null) {
            distribution = distributionMemberSupport.activateOnRecruitPass(member.getId(), nickname);
        } else if (DistributionStatusEnum.APPLY.name().equals(distribution.getDistributionStatus())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "该手机号已有待审核的分销员申请");
        } else if (!DistributionStatusEnum.PASS.name().equals(distribution.getDistributionStatus())) {
            distribution = distributionMemberSupport.activateOnRecruitPass(member.getId(), nickname);
        } else {
            distribution.setMemberName(nickname);
        }

        distribution.setGradeId(String.valueOf(gradeSort));
        gradeService.normalizeGradeReference(distribution);
        distributionMemberSupport.updateById(distribution);

        // 有邀请人时绑定上下级关系并同步客户归属
        if (StringUtils.isNotEmpty(inviterMobile) || StringUtils.isNotEmpty(inviterOpenId)) {
            String parentDistributionId = resolveInviterDistributionId(inviterMobile, inviterOpenId, mobile);
            validateInviteCycle(distribution.getId(), parentDistributionId);
            distributionService.updateParentByAdmin(distribution.getId(), parentDistributionId);
            String operatorId = UserContext.getCurrentUser() == null ? null : UserContext.getCurrentUser().getId();
            customerRelationService.adminRebind(member.getId(), parentDistributionId, "批量导入分销员", operatorId);
        }

        return distribution.getId();
    }

    /**
     * 绑定会员与微信小程序 openid。
     *
     * @param memberId 会员ID
     * @param openId   微信小程序 openid
     */
    private void bindOpenId(String memberId, String openId) {
        if (StringUtils.isEmpty(openId)) {
            return;
        }
        Connect existed = connectService.getOne(new LambdaQueryWrapper<Connect>()
                .eq(Connect::getUnionId, openId)
                .eq(Connect::getUnionType, SourceEnum.WECHAT_MP_OPEN_ID.name())
                .last("limit 1"), false);
        // openid 已被其他会员占用时拒绝绑定
        if (existed != null && !Objects.equals(existed.getUserId(), memberId)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "分销员openid已被其他会员绑定");
        }
        Connect self = connectService.getOne(new LambdaQueryWrapper<Connect>()
                .eq(Connect::getUserId, memberId)
                .eq(Connect::getUnionType, SourceEnum.WECHAT_MP_OPEN_ID.name())
                .last("limit 1"), false);
        if (self == null) {
            connectService.save(new Connect(memberId, openId, SourceEnum.WECHAT_MP_OPEN_ID.name()));
            return;
        }
        if (!Objects.equals(self.getUnionId(), openId)) {
            self.setUnionId(openId);
            connectService.updateById(self);
        }
    }

    /**
     * 根据邀请人手机号或 openid 解析有效分销员ID。
     *
     * @param inviterMobile  邀请人手机号
     * @param inviterOpenId  邀请人 openid
     * @param selfMobile     当前导入行手机号
     * @return 邀请人分销员ID
     */
    private String resolveInviterDistributionId(String inviterMobile, String inviterOpenId, String selfMobile) {
        Member inviterMember = null;
        // 优先按手机号定位邀请人，否则按 openid 反查会员
        if (StringUtils.isNotEmpty(inviterMobile)) {
            if (inviterMobile.equals(selfMobile)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人不能是本人");
            }
            inviterMember = memberService.findByMobile(inviterMobile);
            if (inviterMember == null) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人手机号不存在");
            }
        } else if (StringUtils.isNotEmpty(inviterOpenId)) {
            Connect connect = connectService.getOne(new LambdaQueryWrapper<Connect>()
                    .eq(Connect::getUnionId, inviterOpenId)
                    .eq(Connect::getUnionType, SourceEnum.WECHAT_MP_OPEN_ID.name())
                    .last("limit 1"), false);
            if (connect == null || StringUtils.isEmpty(connect.getUserId())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人openid不存在");
            }
            inviterMember = memberService.getById(connect.getUserId());
            if (inviterMember == null) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人openid对应会员不存在");
            }
            if (selfMobile.equals(inviterMember.getMobile())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人不能是本人");
            }
        }
        if (inviterMember == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人不能为空");
        }
        Distribution inviter = distributionMemberSupport.getByMemberId(inviterMember.getId());
        if (inviter == null || !DistributionStatusEnum.PASS.name().equals(inviter.getDistributionStatus())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人不是有效分销员");
        }
        return inviter.getId();
    }

    /**
     * 校验邀请关系是否形成环（邀请人不能是本人或其下级）。
     *
     * @param childDistributionId  子分销员ID
     * @param parentDistributionId 父分销员ID
     */
    private void validateInviteCycle(String childDistributionId, String parentDistributionId) {
        if (childDistributionId.equals(parentDistributionId)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人不能是本人");
        }
        String current = parentDistributionId;
        Set<String> visited = new HashSet<>();
        // 沿上级链向上追溯，防止形成环（邀请人不能是本人或其下级）
        while (StringUtils.isNotEmpty(current)) {
            if (childDistributionId.equals(current)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人不能为该分销员的下级");
            }
            if (!visited.add(current)) {
                break;
            }
            Distribution parent = distributionService.getById(current);
            current = parent == null ? null : parent.getParentDistributionId();
        }
    }

    /**
     * 从 Excel 行数据填充导入明细字段。
     *
     * @param item 导入明细
     * @param row  Excel 行数据
     */
    private void fillItemFromRow(DistributionImportItem item, List<Object> row) {
        item.setMobile(cell(row, 0));
        item.setOpenId(cell(row, 1));
        item.setNickname(cell(row, 2));
        item.setInviterMobile(cell(row, 3));
        item.setInviterOpenId(cell(row, 4));
        item.setGradeSort(parseGradeSort(cell(row, 5)));
    }

    /**
     * 解析等级序号。
     *
     * @param value 单元格文本
     * @return 等级序号，空值返回 null
     */
    private Integer parseGradeSort(String value) {
        if (StringUtils.isEmpty(value)) {
            return null;
        }
        try {
            return (int) Double.parseDouble(value.trim());
        } catch (Exception e) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "等级序号格式有误");
        }
    }

    /**
     * 读取 Excel 行中指定列的文本值。
     *
     * @param row   Excel 行数据
     * @param index 列索引
     * @return 单元格文本，缺失时返回空字符串
     */
    private String cell(List<Object> row, int index) {
        if (row == null || index >= row.size() || row.get(index) == null) {
            return "";
        }
        return String.valueOf(row.get(index)).trim();
    }

    /**
     * 去除字符串首尾空白，null 转为空字符串。
     *
     * @param value 原始字符串
     * @return 处理后的字符串
     */
    private String trim(String value) {
        return value == null ? "" : value.trim();
    }

    /**
     * 判断 Excel 行是否全为空。
     *
     * @param row Excel 行数据
     * @return 是否为空行
     */
    private boolean isBlankRow(List<Object> row) {
        if (row == null || row.isEmpty()) {
            return true;
        }
        for (Object cell : row) {
            if (cell != null && CharSequenceUtil.isNotBlank(String.valueOf(cell))) {
                return false;
            }
        }
        return true;
    }

    /**
     * 从业务异常中提取导入失败原因。
     *
     * @param ex 业务异常
     * @return 失败原因描述
     */
    private String resolveFailReason(ServiceException ex) {
        if (CharSequenceUtil.isNotBlank(ex.getMsg())) {
            return ex.getMsg();
        }
        if (ex.getResultCode() != null) {
            return ex.getResultCode().message();
        }
        return "导入失败";
    }
}
