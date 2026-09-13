package cn.lili.modules.distribution.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.distribution.entity.dto.DistributionRecruitApplyDTO;
import cn.lili.modules.distribution.support.DistributionSettingSupport;
import cn.lili.modules.system.entity.dto.DistributionSetting;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * 分销总闸 isOpen：与通道 22009/22010 分开，只断言 22000。
 * 
 * @author Mike
 * @date 2026-09-13
 */
class DistributionRecruitServiceImplOpenGateTest {

    @Test
    void submitApplication_throwsDistributionCloseWhenIsOpenFalse() {
        DistributionRecruitServiceImpl service = new DistributionRecruitServiceImpl();
        injectClosedDistribution(service);

        AuthUser user = new AuthUser();
        user.setId("M1");
        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(user);

            assertThatThrownBy(() -> service.submitApplication(new DistributionRecruitApplyDTO()))
                    .isInstanceOf(ServiceException.class)
                    .satisfies(ex -> assertCloseCode((ServiceException) ex));
        }
    }

    @Test
    void getRecruitPage_throwsDistributionCloseWhenIsOpenFalse() {
        DistributionRecruitServiceImpl service = new DistributionRecruitServiceImpl();
        injectClosedDistribution(service);

        assertThatThrownBy(service::getRecruitPage)
                .isInstanceOf(ServiceException.class)
                .satisfies(ex -> assertCloseCode((ServiceException) ex));
    }

    @Test
    void getConditionProgress_throwsDistributionCloseWhenIsOpenFalse() {
        DistributionRecruitServiceImpl service = new DistributionRecruitServiceImpl();
        injectClosedDistribution(service);

        assertThatThrownBy(() -> service.getConditionProgress("M1"))
                .isInstanceOf(ServiceException.class)
                .satisfies(ex -> assertCloseCode((ServiceException) ex));
    }

    private static void injectClosedDistribution(DistributionRecruitServiceImpl service) {
        DistributionSettingSupport settingSupport = mock(DistributionSettingSupport.class);
        DistributionSetting setting = new DistributionSetting();
        setting.setIsOpen(false);
        when(settingSupport.getSetting()).thenReturn(setting);
        ReflectionTestUtils.setField(service, "distributionSettingSupport", settingSupport);
    }

    private static void assertCloseCode(ServiceException ex) {
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.DISTRIBUTION_CLOSE);
        assertThat(ex.getResultCode()).isNotEqualTo(ResultCode.DISTRIBUTION_RETREATED);
        assertThat(ex.getResultCode()).isNotEqualTo(ResultCode.DISTRIBUTION_ONLINE_ENTRY_CLOSED);
    }
}
