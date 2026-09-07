package cn.lili.modules.distribution.support;

import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.mapper.DistributionGradeMapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.Objects;

/**
 * 分销等级引用辅助类。
 * <p>
 * 仅依赖 Mapper，负责等级值解析、默认等级赋值等无事务编排的纯逻辑，
 * 供 MemberSupport 与 GradeService 复用，避免 Service 与 Support 互相注入。
 * </p>
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Component
public class DistributionGradeReferenceSupport {

    public static final int MIN_GRADE_VALUE = 1;
    public static final int MAX_GRADE_VALUE = 5;
    private static final String BASE_GRADE_NAME = "普通分销员";

    @Autowired
    private DistributionGradeMapper distributionGradeMapper;

    /**
     * 确保存在基础等级（普通分销员）。
     */
    public void ensureBaseGrade() {
        long baseCount = distributionGradeMapper.selectCount(new LambdaQueryWrapper<DistributionGrade>()
                .eq(DistributionGrade::getSortOrder, MIN_GRADE_VALUE));
        if (baseCount > 0) {
            return;
        }
        // 首次使用时插入「普通分销员」作为 1 级默认等级
        DistributionGrade grade = new DistributionGrade();
        grade.setGradeName(BASE_GRADE_NAME);
        grade.setSortOrder(MIN_GRADE_VALUE);
        grade.setSalesThreshold(BigDecimal.ZERO);
        grade.setCommissionThreshold(BigDecimal.ZERO);
        grade.setInviteThreshold(0);
        grade.setEnableSalesCondition(false);
        grade.setEnableCommissionCondition(false);
        grade.setEnableInviteCondition(false);
        grade.setGoodsCommissionRate(new BigDecimal("0.1000"));
        grade.setInviteCommissionRate(BigDecimal.ZERO);
        distributionGradeMapper.insert(grade);
    }

    /**
     * 按等级ID或等级序号解析等级实体。
     *
     * @param gradeRef 等级ID或1-5的等级序号
     * @return 等级实体，不存在返回 null
     */
    public DistributionGrade resolveGrade(String gradeRef) {
        if (gradeRef == null || gradeRef.isBlank()) {
            return null;
        }
        if (isGradeValue(gradeRef)) {
            ensureBaseGrade();
            // 1-5 序号按 sort_order 查，兼容历史仅存序号不落库 ID 的场景
            return distributionGradeMapper.selectOne(new LambdaQueryWrapper<DistributionGrade>()
                    .eq(DistributionGrade::getSortOrder, Integer.parseInt(gradeRef))
                    .last("limit 1"));
        }
        return distributionGradeMapper.selectById(gradeRef);
    }

    /**
     * 将分销员等级引用统一为等级序号字符串。
     *
     * @param distribution 分销员实体
     */
    public void normalizeGradeReference(Distribution distribution) {
        if (distribution == null || distribution.getGradeId() == null || distribution.getGradeId().isBlank()) {
            return;
        }
        DistributionGrade grade = resolveGrade(distribution.getGradeId());
        if (grade == null || grade.getSortOrder() == null) {
            return;
        }
        // 统一存等级序号字符串，避免 ID 与序号混用
        String gradeValue = formatGradeValue(grade.getSortOrder());
        if (!Objects.equals(gradeValue, distribution.getGradeId())) {
            distribution.setGradeId(gradeValue);
        }
    }

    /**
     * 将等级排序值格式化为等级序号字符串。
     *
     * @param sortOrder 等级排序
     * @return 等级序号
     */
    public String formatGradeValue(Integer sortOrder) {
        return sortOrder == null ? null : String.valueOf(sortOrder);
    }

    /**
     * 分销员未设置等级时分配基础等级。
     *
     * @param distribution 分销员实体
     */
    public void assignBaseGradeIfAbsent(Distribution distribution) {
        if (distribution == null) {
            return;
        }
        if (distribution.getGradeId() != null && !distribution.getGradeId().isBlank()) {
            normalizeGradeReference(distribution);
            return;
        }
        // 未设置等级时默认 1 级
        distribution.setGradeId(formatGradeValue(MIN_GRADE_VALUE));
    }

    /**
     * 判断等级引用是否为 1-5 的等级序号。
     *
     * @param gradeRef 等级ID或等级序号
     * @return 是否为等级序号
     */
    private boolean isGradeValue(String gradeRef) {
        if (gradeRef == null || gradeRef.isBlank()) {
            return false;
        }
        if (!gradeRef.chars().allMatch(Character::isDigit)) {
            return false;
        }
        try {
            int value = Integer.parseInt(gradeRef);
            return value >= MIN_GRADE_VALUE && value <= MAX_GRADE_VALUE;
        } catch (NumberFormatException ex) {
            return false;
        }
    }
}
