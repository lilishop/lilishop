package cn.lili.modules.distribution.support;

import cn.lili.modules.distribution.entity.enums.RecruitApplyModeEnum;
import cn.lili.modules.distribution.entity.enums.RecruitJoinConditionTypeEnum;
import cn.lili.modules.system.entity.dto.DistributionRecruitFieldSetting;
import cn.lili.modules.system.entity.dto.DistributionRecruitSetting;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;

/**
 * AUTO 模式与申请表单互斥。
 *
 * @author Mike
 * @date 2026-09-18
 */
class DistributionRecruitSettingSupportAutoModeTest {

    private final DistributionRecruitSettingSupport support = new DistributionRecruitSettingSupport();

    /**
     * AUTO 校验时强制关闭「需填写申请资料」，且不抛错。
     */
    @Test
    void validate_autoMode_forcesRequireApplyInfoFalse() {
        DistributionRecruitSetting setting = conditionalBase();
        setting.setApplyMode(RecruitApplyModeEnum.AUTO.name());
        setting.setRequireApplyInfo(true);
        setting.setFields(new ArrayList<>());

        assertThatCode(() -> support.validate(setting)).doesNotThrowAnyException();
        assertThat(setting.getRequireApplyInfo()).isFalse();
    }

    /**
     * AUTO 规范化时清空申请资料开关与自定义字段。
     */
    @Test
    void normalize_autoMode_clearsApplyInfo() {
        DistributionRecruitSetting setting = new DistributionRecruitSetting();
        setting.setApplyMode(RecruitApplyModeEnum.AUTO.name());
        setting.setRequireApplyInfo(true);
        DistributionRecruitFieldSetting field = new DistributionRecruitFieldSetting();
        field.setFieldName("姓名");
        field.setFieldType("NAME");
        setting.setFields(List.of(field));

        support.normalize(setting);

        assertThat(setting.getRequireApplyInfo()).isFalse();
        assertThat(setting.getFields()).isEmpty();
    }

    private static DistributionRecruitSetting conditionalBase() {
        DistributionRecruitSetting setting = new DistributionRecruitSetting();
        setting.setJoinConditionType(RecruitJoinConditionTypeEnum.CONDITIONAL.name());
        setting.setRequireSelfPurchaseAmount(true);
        setting.setMinSelfPurchaseAmount(new BigDecimal("0.01"));
        return setting;
    }
}
