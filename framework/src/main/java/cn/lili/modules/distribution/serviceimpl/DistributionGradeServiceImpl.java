package cn.lili.modules.distribution.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.entity.dos.DistributionGradeHistory;
import cn.lili.modules.distribution.mapper.DistributionGradeHistoryMapper;
import cn.lili.modules.distribution.mapper.DistributionGradeMapper;
import cn.lili.modules.distribution.mapper.DistributionMapper;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.support.DistributionGradeReferenceSupport;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * 分销等级服务实现
 *
 * @author pikachu
 */
@Service
public class DistributionGradeServiceImpl extends ServiceImpl<DistributionGradeMapper, DistributionGrade>
        implements DistributionGradeService {

    private static final int MAX_GRADE_COUNT = 5;

    @Autowired
    private DistributionGradeHistoryMapper gradeHistoryMapper;

    @Autowired
    private DistributionMapper distributionMapper;

    @Autowired
    private DistributionGradeReferenceSupport gradeReferenceSupport;

    @Override
    public List<DistributionGrade> listGrades() {
        gradeReferenceSupport.ensureBaseGrade();
        return list(new LambdaQueryWrapper<DistributionGrade>()
                .orderByAsc(DistributionGrade::getSortOrder));
    }

    @Override
    public List<DistributionGrade> listBuyerGrades() {
        return listGrades();
    }

    @Override
    public DistributionGrade getBaseGrade() {
        gradeReferenceSupport.ensureBaseGrade();
        return getOne(new LambdaQueryWrapper<DistributionGrade>()
                .eq(DistributionGrade::getSortOrder, DistributionGradeReferenceSupport.MIN_GRADE_VALUE)
                .last("limit 1"));
    }

    @Override
    public DistributionGrade resolveGrade(String gradeRef) {
        return gradeReferenceSupport.resolveGrade(gradeRef);
    }

    @Override
    public DistributionGrade resolveGradeWithFallback(String gradeRef) {
        DistributionGrade grade = resolveGrade(gradeRef);
        if (grade != null) {
            return grade;
        }
        // 逐级回退：基础等级 -> 列表首项 -> 零佣金占位对象
        if (gradeRef == null || gradeRef.isBlank()) {
            grade = resolveGrade(formatGradeValue(DistributionGradeReferenceSupport.MIN_GRADE_VALUE));
            if (grade != null) {
                return grade;
            }
        }
        List<DistributionGrade> grades = listGrades();
        if (!grades.isEmpty()) {
            return grades.get(0);
        }
        DistributionGrade fallback = new DistributionGrade();
        fallback.setGoodsCommissionRate(BigDecimal.ZERO);
        fallback.setInviteCommissionRate(BigDecimal.ZERO);
        return fallback;
    }

    @Override
    public void normalizeGradeReference(Distribution distribution) {
        gradeReferenceSupport.normalizeGradeReference(distribution);
    }

    @Override
    public String formatGradeValue(Integer sortOrder) {
        return gradeReferenceSupport.formatGradeValue(sortOrder);
    }

    @Override
    public void assignBaseGradeIfAbsent(Distribution distribution) {
        gradeReferenceSupport.assignBaseGradeIfAbsent(distribution);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveAllGrades(List<DistributionGrade> grades) {
        if (grades == null || grades.isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请至少添加一个分销等级");
        }
        if (grades.size() > MAX_GRADE_COUNT) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "最多添加5个分销等级");
        }
        List<DistributionGrade> normalized = new java.util.ArrayList<>();
        for (DistributionGrade grade : grades) {
            validateGradeValueUniqueInBatch(grade, grades);
            normalizeUpgradeConditions(grade);
            validateGrade(grade);
            normalized.add(grade);
        }
        // 批量校验等级递进链与跨等级佣金上限
        validateGradeProgressionChain(normalized);
        validateCrossRates(normalized);
        for (DistributionGrade grade : normalized) {
            if (grade.getId() != null && !grade.getId().isBlank()) {
                updateById(grade);
            } else {
                save(grade);
            }
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteGrade(String id) {
        DistributionGrade grade = getById(id);
        if (grade != null && isBaseGrade(grade)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "默认分销等级不能删除");
        }
        removeById(id);
    }

    @Override
    public void validateCrossRates(List<DistributionGrade> grades) {
        BigDecimal maxGoods = BigDecimal.ZERO;
        BigDecimal maxInvite = BigDecimal.ZERO;
        // 取各等级商品/邀请佣金比例的最大值，合计不得超过 100%
        for (DistributionGrade grade : grades) {
            if (grade.getGoodsCommissionRate() != null && grade.getGoodsCommissionRate().compareTo(maxGoods) > 0) {
                maxGoods = grade.getGoodsCommissionRate();
            }
            if (grade.getInviteCommissionRate() != null && grade.getInviteCommissionRate().compareTo(maxInvite) > 0) {
                maxInvite = grade.getInviteCommissionRate();
            }
        }
        if (maxGoods.add(maxInvite).compareTo(BigDecimal.ONE) > 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "商品佣金比例与邀请佣金比例之和不能超过100%");
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void refreshGrade(String distributionId) {
        Distribution distribution = distributionMapper.selectById(distributionId);
        if (distribution == null) {
            return;
        }
        String previousGradeId = distribution.getGradeId();
        normalizeGradeReference(distribution);
        // 等级引用修正后先持久化，再尝试自动升级
        if (!Objects.equals(previousGradeId, distribution.getGradeId())) {
            distributionMapper.updateById(distribution);
        }
        List<DistributionGrade> grades = listGrades();
        if (grades.isEmpty()) {
            return;
        }
        DistributionGrade current = resolveGrade(distribution.getGradeId());
        int currentSort = current == null ? -1 : defaultSort(current);
        DistributionGrade target = null;
        // 遍历高于当前等级的配置，取满足条件的最高等级
        for (DistributionGrade grade : grades) {
            if (defaultSort(grade) <= currentSort) {
                continue;
            }
            if (matchGrade(distribution, grade)) {
                target = grade;
            }
        }
        if (target == null || (current != null && Objects.equals(current.getSortOrder(), target.getSortOrder()))) {
            return;
        }
        distribution.setGradeId(formatGradeValue(target.getSortOrder()));
        distributionMapper.updateById(distribution);
        // 记录升级历史
        DistributionGradeHistory history = new DistributionGradeHistory();
        history.setDistributionId(distributionId);
        history.setPreviousGradeId(current == null ? null : formatGradeValue(current.getSortOrder()));
        history.setGradeId(formatGradeValue(target.getSortOrder()));
        history.setGradeName(target.getGradeName());
        history.setGoodsCommissionRate(target.getGoodsCommissionRate());
        history.setInviteCommissionRate(target.getInviteCommissionRate());
        history.setUpgradeTime(new Date());
        gradeHistoryMapper.insert(history);
    }

    /**
     * 校验分销等级基础规则。
     *
     * @param grade 分销等级
     */
    private void validateGrade(DistributionGrade grade) {
        Integer gradeValue = grade.getSortOrder();
        if (gradeValue == null || gradeValue < DistributionGradeReferenceSupport.MIN_GRADE_VALUE
                || gradeValue > DistributionGradeReferenceSupport.MAX_GRADE_VALUE) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "等级值必须为1-5");
        }
        validateRateRange(grade.getGoodsCommissionRate(), "商品佣金比例");
        validateRateRange(grade.getInviteCommissionRate(), "邀请佣金比例");
        if (defaultDecimal(grade.getGoodsCommissionRate()).add(defaultDecimal(grade.getInviteCommissionRate()))
                .compareTo(BigDecimal.ONE) > 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "单等级商品佣金比例与邀请佣金比例之和不能超过100%");
        }
        if (!isConditionEnabled(grade.getEnableSalesCondition())
                && !isConditionEnabled(grade.getEnableCommissionCondition())
                && !isConditionEnabled(grade.getEnableInviteCondition())) {
            // 非基础等级须至少启用一项升级规则
            if (!isBaseGrade(grade)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "请至少启用一项升级规则");
            }
        } else {
            validateEnabledThresholds(grade);
        }
    }

    /**
     * 校验单个比例为 0～1 小数（页面 100% 对应入库 1）。
     *
     * @param rate      比例
     * @param fieldName 字段名称（用于对外文案）
     */
    private void validateRateRange(BigDecimal rate, String fieldName) {
        if (rate == null) {
            return;
        }
        if (rate.compareTo(BigDecimal.ZERO) < 0 || rate.compareTo(BigDecimal.ONE) > 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, fieldName + "不能超过100%");
        }
    }

    /**
     * 校验已启用升级条件的阈值。
     *
     * @param grade 分销等级
     */
    private void validateEnabledThresholds(DistributionGrade grade) {
        if (isConditionEnabled(grade.getEnableSalesCondition())
                && defaultDecimal(grade.getSalesThreshold()).compareTo(BigDecimal.ZERO) <= 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "销售额必须大于0");
        }
        if (isConditionEnabled(grade.getEnableCommissionCondition())
                && defaultDecimal(grade.getCommissionThreshold()).compareTo(BigDecimal.ZERO) <= 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "收益额必须大于0");
        }
        if (isConditionEnabled(grade.getEnableInviteCondition())
                && defaultInt(grade.getInviteThreshold()) <= 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "邀请人数必须大于0");
        }
    }

    /**
     * 校验批量保存时等级值的唯一性。
     *
     * @param grade  当前等级
     * @param grades 同批次等级列表
     */
    private void validateGradeValueUniqueInBatch(DistributionGrade grade, List<DistributionGrade> grades) {
        long duplicateInBatch = grades.stream()
                .filter(item -> Objects.equals(item.getSortOrder(), grade.getSortOrder()))
                .count();
        if (duplicateInBatch > 1) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "等级值不能重复");
        }
        List<String> batchIds = grades.stream()
                .map(DistributionGrade::getId)
                .filter(id -> id != null && !id.isBlank())
                .toList();
        long duplicateInDatabase = count(new LambdaQueryWrapper<DistributionGrade>()
                .eq(DistributionGrade::getSortOrder, grade.getSortOrder())
                .notIn(!batchIds.isEmpty(), DistributionGrade::getId, batchIds));
        if (duplicateInDatabase > 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "等级值已存在，请选择其他等级值");
        }
    }

    /**
     * 判断分销商是否满足指定等级的升级条件（已启用条件须全部达标）。
     *
     * @param distribution 分销商
     * @param grade        目标等级
     * @return 是否满足升级条件
     */
    private boolean matchGrade(Distribution distribution, DistributionGrade grade) {
        boolean hasEnabledCondition = false;
        boolean allEnabledMatched = true;
        // 已启用的升级条件须全部达标（AND 关系）
        if (isConditionEnabled(grade.getEnableSalesCondition())) {
            hasEnabledCondition = true;
            boolean salesOk = distribution.getValidSalesAmount() != null
                    && distribution.getValidSalesAmount().compareTo(defaultDecimal(grade.getSalesThreshold())) >= 0;
            allEnabledMatched = allEnabledMatched && salesOk;
        }
        if (isConditionEnabled(grade.getEnableCommissionCondition())) {
            hasEnabledCondition = true;
            boolean commissionOk = distribution.getValidCommissionAmount() != null
                    && distribution.getValidCommissionAmount().compareTo(defaultDecimal(grade.getCommissionThreshold())) >= 0;
            allEnabledMatched = allEnabledMatched && commissionOk;
        }
        if (isConditionEnabled(grade.getEnableInviteCondition())) {
            hasEnabledCondition = true;
            boolean inviteOk = distribution.getInviteCount() != null
                    && distribution.getInviteCount() >= defaultInt(grade.getInviteThreshold());
            allEnabledMatched = allEnabledMatched && inviteOk;
        }
        return hasEnabledCondition && allEnabledMatched;
    }

    /**
     * 校验等级链的递进规则。
     *
     * @param grades 等级列表
     */
    private void validateGradeProgressionChain(List<DistributionGrade> grades) {
        if (grades == null || grades.size() < 2) {
            return;
        }
        grades.sort(Comparator.comparingInt(this::defaultSort));
        // 相邻等级间校验升级条件的启用一致性与阈值递增
        for (int i = 1; i < grades.size(); i++) {
            validateComparedGrades(grades.get(i - 1), grades.get(i));
        }
    }

    /**
     * 校验相邻两个等级的递进约束。
     *
     * @param lower  较低等级
     * @param higher 较高等级
     */
    private void validateComparedGrades(DistributionGrade lower, DistributionGrade higher) {
        String lowerLabel = gradeLabel(lower);
        String higherLabel = gradeLabel(higher);
        if (isConditionEnabled(lower.getEnableSalesCondition())) {
            if (!isConditionEnabled(higher.getEnableSalesCondition())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR,
                        higherLabel + "必须启用" + lowerLabel + "已启用的销售额升级条件");
            }
            if (defaultDecimal(higher.getSalesThreshold()).compareTo(defaultDecimal(lower.getSalesThreshold())) < 0) {
                throw new ServiceException(ResultCode.PARAMS_ERROR,
                        higherLabel + "销售额阈值不能小于" + lowerLabel);
            }
        }
        if (isConditionEnabled(lower.getEnableCommissionCondition())) {
            if (!isConditionEnabled(higher.getEnableCommissionCondition())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR,
                        higherLabel + "必须启用" + lowerLabel + "已启用的收益额升级条件");
            }
            if (defaultDecimal(higher.getCommissionThreshold()).compareTo(defaultDecimal(lower.getCommissionThreshold())) < 0) {
                throw new ServiceException(ResultCode.PARAMS_ERROR,
                        higherLabel + "收益额阈值不能小于" + lowerLabel);
            }
        }
        if (isConditionEnabled(lower.getEnableInviteCondition())) {
            if (!isConditionEnabled(higher.getEnableInviteCondition())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR,
                        higherLabel + "必须启用" + lowerLabel + "已启用的邀请人数升级条件");
            }
            if (defaultInt(higher.getInviteThreshold()) < defaultInt(lower.getInviteThreshold())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR,
                        higherLabel + "邀请人数阈值不能小于" + lowerLabel);
            }
        }
    }

    /**
     * 获取等级展示名称。
     *
     * @param grade 分销等级
     * @return 等级名称或等级值标签
     */
    private String gradeLabel(DistributionGrade grade) {
        if (grade.getGradeName() != null && !grade.getGradeName().trim().isEmpty()) {
            return grade.getGradeName().trim();
        }
        return "等级值" + defaultSort(grade);
    }

    /**
     * 规范化升级条件开关。
     *
     * @param grade 分销等级
     */
    private void normalizeUpgradeConditions(DistributionGrade grade) {
        // 基础等级无升级条件，其他等级默认三项条件均启用
        if (isBaseGrade(grade)) {
            grade.setEnableSalesCondition(false);
            grade.setEnableCommissionCondition(false);
            grade.setEnableInviteCondition(false);
            return;
        }
        if (grade.getEnableSalesCondition() == null) {
            grade.setEnableSalesCondition(true);
        }
        if (grade.getEnableCommissionCondition() == null) {
            grade.setEnableCommissionCondition(true);
        }
        if (grade.getEnableInviteCondition() == null) {
            grade.setEnableInviteCondition(true);
        }
    }

    /**
     * 判断升级条件是否启用（仅显式开启时生效）。
     *
     * @param enabled 条件开关
     * @return 是否启用
     */
    private boolean isConditionEnabled(Boolean enabled) {
        return Boolean.TRUE.equals(enabled);
    }

    /**
     * 判断是否为默认基础等级。
     *
     * @param grade 分销等级
     * @return 是否为基础等级
     */
    private boolean isBaseGrade(DistributionGrade grade) {
        return grade != null && Objects.equals(grade.getSortOrder(), DistributionGradeReferenceSupport.MIN_GRADE_VALUE);
    }

    /**
     * 获取等级排序值，空值默认为0。
     *
     * @param grade 分销等级
     * @return 排序值
     */
    private int defaultSort(DistributionGrade grade) {
        return grade.getSortOrder() == null ? 0 : grade.getSortOrder();
    }

    /**
     * 获取BigDecimal默认值，空值归零。
     *
     * @param value 数值
     * @return 非空数值
     */
    private BigDecimal defaultDecimal(BigDecimal value) {
        return value == null ? BigDecimal.ZERO : value;
    }

    /**
     * 获取整型默认值，空值归零。
     *
     * @param value 数值
     * @return 非空整型
     */
    private int defaultInt(Integer value) {
        return value == null ? 0 : value;
    }
}
