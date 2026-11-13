package cn.lili.modules.member.serviceimpl;

import cn.lili.modules.member.service.MemberBenefitQueryService;

import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberGrade;
import cn.lili.modules.member.entity.vo.MemberCurrentBenefitVO;
import cn.lili.modules.member.service.MemberGradeService;
import cn.lili.modules.member.service.MemberService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;
import java.util.List;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;

/**
 * 当前普通客户等级与升级经验进度测试，覆盖下一等级筛选、默认等级基线、最高等级和异常经验值。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
class MemberCurrentBenefitTest {
    /**
     * 模拟客户资料及当前经验查询。
     */
    private final MemberService members = mock(MemberService.class);
    /**
     * 模拟绑定等级、经验回退等级及升级候选查询。
     */
    private final MemberGradeService grades = mock(MemberGradeService.class);
    /** 模拟按当前等级配置查询已开启权益。 */
    private final MemberBenefitQueryService queries = mock(MemberBenefitQueryService.class);
    /**
     * 被测试的客户权益服务，成长进度使用真实实现。
     */
    private final MemberBenefitServiceImpl service = new MemberBenefitServiceImpl();
    /**
     * 各测试场景复用的客户资料样本。
     */
    private final Member member = new Member();

    /**
     * 注入模拟客户、等级及公共权益查询服务，准备已绑定白银等级的客户样本。
     */
    @BeforeEach void setup() {
        ReflectionTestUtils.setField(service, "memberService", members);
        ReflectionTestUtils.setField(service, "benefitQueries", queries);
        ReflectionTestUtils.setField(service, "memberGradeService", grades);
        member.setId("member"); member.setGradeId("silver");
        when(members.getById("member")).thenReturn(member);
    }

    /**
     * 构造客户等级样本，便于测试经验门槛、默认标记与启停状态。
     *
     * @param id 等级主键，同时作为测试等级名称
     * @param threshold 升级所需累计经验值
     * @param isDefault 是否为默认等级
     * @param state 等级状态，OPEN启用、CLOSE停用
     * @return 用于查询回显或升级候选的等级
     */
    private MemberGrade grade(String id, int threshold, boolean isDefault, String state) {
        MemberGrade value = new MemberGrade();
        value.setId(id); value.setGradeName(id); value.setRequiredExperience(threshold);
        value.setIsDefault(isDefault); value.setGradeState(state);
        return value;
    }

    /**
     * 配置当前等级、客户经验及候选等级，并调用真实的当前权益查询逻辑。
     *
     * @param current 客户当前绑定的等级，可为null
     * @param experience 当前累计经验值，可为null
     * @param candidates 待筛选的升级候选等级
     * @return 当前等级权益及成长进度响应
     */
    private MemberCurrentBenefitVO current(MemberGrade current, Long experience, MemberGrade... candidates) {
        member.setExperience(experience);
        when(grades.getById("silver")).thenReturn(current);
        when(grades.listForUpgrade()).thenReturn(List.of(candidates));
        return service.getCurrentMemberBenefits("member");
    }

    /**
     * 验证下一等级按更高经验门槛中的最小值确定，忽略候选顺序和已停用等级。
     * 白银门槛1000、黄金门槛5000、当前经验3000时，差2000经验，区间进度为50%。
     */
    @Test void findsNearestEnabledThresholdRegardlessOfDisplayOrder() {
        var silver = grade("silver", 1000, false, "OPEN");
        var gold = grade("gold", 5000, false, "OPEN");
        var result = current(silver, 3000L, grade("diamond", 15000, false, "OPEN"),
                grade("closed", 4000, false, "CLOSE"), gold, silver);
        assertThat(result.getExperience()).isEqualTo(3000);
        assertThat(result.getNextGrade()).isSameAs(gold);
        assertThat(result.getRemainingExperience()).isEqualTo(2000);
        assertThat(result.getUpgradeProgress()).isEqualTo(50);
        assertThat(result.getHighestGrade()).isFalse();
    }

    /**
     * 验证默认等级从0经验计算进度，缺失或负数经验按0展示，500经验对应升至1000门槛的50%。
     */
    @Test void defaultGradeStartsProgressFromZeroAndNormalizesMissingOrNegativeExperience() {
        for (Long experience : new Long[]{null, -10L, 0L}) {
            var result = current(grade("ordinary", 1, true, "OPEN"), experience, grade("silver", 1000, false, "OPEN"));
            assertThat(result.getExperience()).isZero();
            assertThat(result.getRemainingExperience()).isEqualTo(1000);
            assertThat(result.getUpgradeProgress()).isZero();
        }
        var result = current(grade("ordinary", 1, true, "OPEN"), 500L, grade("silver", 1000, false, "OPEN"));
        assertThat(result.getUpgradeProgress()).isEqualTo(50);
    }

    /**
     * 验证最高启用等级没有下一目标，所差经验为0、进度为100，不把更高但停用的等级作为目标。
     */
    @Test void highestEnabledGradeHasNoNextTarget() {
        var diamond = grade("diamond", 15000, false, "OPEN");
        var result = current(diamond, 20000L, diamond, grade("closed", 30000, false, "CLOSE"));
        assertThat(result.getNextGrade()).isNull();
        assertThat(result.getRemainingExperience()).isZero();
        assertThat(result.getUpgradeProgress()).isEqualTo(100);
        assertThat(result.getHighestGrade()).isTrue();
    }

    /**
     * 验证指定等级下的进度限制在0～100；经验低于当前门槛归零，达到目标或极大经验值时封顶。
     * 存在下一等级时，即使经验已达门槛也不直接标记为最高等级。
     */
    @Test void clampsProgressForAssignedGradesAndPendingUpgrade() {
        var silver = grade("silver", 1000, false, "OPEN");
        var gold = grade("gold", 5000, false, "OPEN");
        assertThat(current(silver, 100L, gold).getUpgradeProgress()).isZero();
        for (long experience : new long[]{5000L, Long.MAX_VALUE}) {
            var result = current(silver, experience, gold);
            assertThat(result.getRemainingExperience()).isZero();
            assertThat(result.getUpgradeProgress()).isEqualTo(100);
            assertThat(result.getHighestGrade()).isFalse();
        }
    }

    /**
     * 验证客户绑定等级停用后按经验回退到有效等级，并使用回退等级重新计算升级进度。
     */
    @Test void disabledCurrentGradeUsesExperienceFallback() {
        var ordinary = grade("ordinary", 1, true, "OPEN");
        when(grades.getCurrentGradeByExperience(100L)).thenReturn(ordinary);
        var result = current(grade("silver", 1000, false, "CLOSE"), 100L, grade("silver", 1000, false, "OPEN"));
        assertThat(result.getGradeId()).isEqualTo("ordinary");
        assertThat(result.getRemainingExperience()).isEqualTo(900);
        assertThat(result.getUpgradeProgress()).isEqualTo(10);
    }

    /**
     * 验证缺少当前等级时不误判为最高等级，下一等级为空，进度为0。
     */
    @Test void absentGradeIsNotHighestGrade() {
        var result = current(null, null);
        assertThat(result.getGrade()).isNull();
        assertThat(result.getNextGrade()).isNull();
        assertThat(result.getHighestGrade()).isFalse();
        assertThat(result.getUpgradeProgress()).isZero();
    }

    /** 验证等级权益关联去除空白条目和首尾空格，查询时仍限定仅返回开启权益。 */
    @Test void trimsBenefitIdsAndIgnoresBlankEntries() {
        var silver = grade("silver", 1000, false, "OPEN");
        silver.setBenefitIds(" first , , second , ");
        current(silver, 1000L);
        verify(queries).listByIdList(List.of("first", "second"), true);
    }
}
