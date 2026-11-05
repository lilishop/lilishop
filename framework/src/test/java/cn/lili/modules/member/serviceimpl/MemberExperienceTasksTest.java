package cn.lili.modules.member.serviceimpl;

import cn.lili.modules.member.mapper.MemberExperienceLogMapper;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.service.SettingService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;

/**
 * 买家端经验值任务查询测试，验证仅返回已开启规则及指定客户的累计经验，兼容设置缺失。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
class MemberExperienceTasksTest {
    /**
     * 模拟li_setting中的经验值规则配置。
     */
    private final SettingService settings = mock(SettingService.class);
    /**
     * 模拟按客户与规则汇总的经验流水。
     */
    private final MemberExperienceLogMapper logs = mock(MemberExperienceLogMapper.class);
    /**
     * 被测试的经验任务查询服务。
     */
    private final MemberExperienceServiceImpl service = new MemberExperienceServiceImpl();

    /**
     * 注入经验规则设置及客户经验流水的模拟查询依赖。
     */
    @BeforeEach void setup() {
        ReflectionTestUtils.setField(service, "settingService", settings);
        ReflectionTestUtils.setField(service, "memberExperienceLogMapper", logs);
    }

    /**
     * 验证仅返回enabled为true且规则编码有效的任务，关闭、未明确开启及空规则均被过滤。
     * 累计经验、记录次数按传入客户查询，返回任务奖励、累计限额和经验说明。
     */
    @Test void returnsOnlyEnabledRulesAndProgressBelongingToCurrentMember() {
        Setting setting = new Setting();
        setting.setSettingValue("""
            {"description":"完成任务积累经验","items":[
              {"ruleKey":"SIGN_IN","ruleName":"每日签到","enabled":true,"value":5,"maxValue":100},
              {"ruleKey":"COMMENT","ruleName":"评价","enabled":false,"value":10},
              {"ruleKey":"SHARE","ruleName":"分享","value":8},
              {"ruleKey":"","enabled":true},null]}
            """);
        when(settings.get("EXPERIENCE_SETTING")).thenReturn(setting);
        when(logs.sumByMemberAndRule("member-a", "SIGN_IN")).thenReturn(15L);
        when(logs.countByMemberAndRule("member-a", "SIGN_IN")).thenReturn(3L);
        var result = service.getEnabledRuleProgress("member-a");
        assertThat(result.getDescription()).isEqualTo("完成任务积累经验");
        assertThat(result.getItems()).hasSize(1);
        var row = result.getItems().getFirst();
        assertThat(row.getRuleKey()).isEqualTo("SIGN_IN");
        assertThat(row.getValue()).isEqualTo(5);
        assertThat(row.getMaxValue()).isEqualTo(100);
        assertThat(row.getGainedExperience()).isEqualTo(15);
        verify(logs, never()).sumByMemberAndRule(anyString(), eq("COMMENT"));
        verify(logs, never()).sumByMemberAndRule(anyString(), eq("SHARE"));
    }

    /**
     * 验证经验设置缺失或客户标识空白时返回空任务列表，且不查询经验流水。
     */
    @Test void missingSettingsOrMemberReturnsEmptyTasks() {
        assertThat(service.getEnabledRuleProgress("member-a").getItems()).isEmpty();
        assertThat(service.getEnabledRuleProgress("").getItems()).isEmpty();
        verifyNoInteractions(logs);
    }
}
