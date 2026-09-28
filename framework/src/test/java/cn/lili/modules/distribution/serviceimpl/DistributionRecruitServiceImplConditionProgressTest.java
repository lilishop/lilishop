package cn.lili.modules.distribution.serviceimpl;

import cn.lili.modules.distribution.entity.vos.DistributionRecruitConditionProgressVO;
import cn.lili.modules.distribution.support.DistributionRecruitSettingSupport;
import cn.lili.modules.distribution.support.DistributionSettingSupport;
import cn.lili.modules.system.entity.dto.DistributionRecruitSetting;
import cn.lili.modules.system.entity.dto.DistributionSetting;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 未配置招募方案时进度不应视为已达标。
 *
 * @author Mike
 * @date 2026-09-18
 */
class DistributionRecruitServiceImplConditionProgressTest {

    /**
     * 方案版本为 0（未配置招募）时，进度判定为未达标。
     */
    @Test
    void getConditionProgress_planVersionZero_notSatisfied() {
        DistributionRecruitServiceImpl service = new DistributionRecruitServiceImpl();
        DistributionSettingSupport settingSupport = mock(DistributionSettingSupport.class);
        DistributionSetting open = new DistributionSetting();
        open.setIsOpen(true);
        when(settingSupport.getSetting()).thenReturn(open);
        ReflectionTestUtils.setField(service, "distributionSettingSupport", settingSupport);

        DistributionRecruitSettingSupport recruitSettingSupport = mock(DistributionRecruitSettingSupport.class);
        DistributionRecruitSetting setting = new DistributionRecruitSetting();
        setting.setPlanVersion(0);
        when(recruitSettingSupport.readSetting()).thenReturn(setting);
        ReflectionTestUtils.setField(service, "recruitSettingSupport", recruitSettingSupport);

        DistributionRecruitConditionProgressVO progress = service.getConditionProgress("M1");
        assertThat(progress.getSatisfied()).isFalse();
    }
}
