package cn.lili.modules.distribution.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dto.DistributionRecruitApplyDTO;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.support.DistributionMemberSupport;
import cn.lili.modules.distribution.support.DistributionSettingSupport;
import cn.lili.modules.system.entity.dto.DistributionRecruitSetting;
import cn.lili.modules.system.entity.dto.DistributionSetting;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * 清退后提交招募申请：独立错误码，不复用已申请文案。
 *
 * @author Mike
 * @date 2026-09-09
 */
class DistributionRecruitServiceImplSubmitApplicationTest {

    @Test
    void submitApplication_throwsIndependentCodeWhenRetreated() {
        DistributionRecruitServiceImpl service = spy(new DistributionRecruitServiceImpl());
        DistributionMemberSupport memberSupport = mock(DistributionMemberSupport.class);
        ReflectionTestUtils.setField(service, "distributionMemberSupport", memberSupport);
        injectOpenDistribution(service);

        DistributionRecruitSetting setting = new DistributionRecruitSetting();
        setting.setPlanVersion(1);
        doReturn(setting).when(service).getSetting();

        Distribution retreated = new Distribution();
        retreated.setMemberId("M1");
        retreated.setDistributionStatus(DistributionStatusEnum.RETREAT.name());
        when(memberSupport.getByMemberId("M1")).thenReturn(retreated);
        when(memberSupport.isBlockingRecruitApply(retreated)).thenReturn(true);

        AuthUser user = new AuthUser();
        user.setId("M1");
        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(user);

            assertThatThrownBy(() -> service.submitApplication(new DistributionRecruitApplyDTO()))
                    .isInstanceOf(ServiceException.class)
                    .satisfies(ex -> {
                        ServiceException serviceException = (ServiceException) ex;
                        assertThat(serviceException.getResultCode()).isEqualTo(ResultCode.DISTRIBUTION_RETREATED);
                        assertThat(serviceException.getResultCode()).isNotEqualTo(ResultCode.DISTRIBUTION_IS_APPLY);
                        assertThat(serviceException.getResultCode().message())
                                .isEqualTo("您的分销资格已被清退。请联系管理员！");
                    });
        }
    }

    @Test
    void submitApplication_reusesApplyCodeWhenAlreadyPass() {
        DistributionRecruitServiceImpl service = spy(new DistributionRecruitServiceImpl());
        DistributionMemberSupport memberSupport = mock(DistributionMemberSupport.class);
        ReflectionTestUtils.setField(service, "distributionMemberSupport", memberSupport);
        injectOpenDistribution(service);

        DistributionRecruitSetting setting = new DistributionRecruitSetting();
        setting.setPlanVersion(1);
        doReturn(setting).when(service).getSetting();

        Distribution pass = new Distribution();
        pass.setMemberId("M1");
        pass.setDistributionStatus(DistributionStatusEnum.PASS.name());
        when(memberSupport.getByMemberId("M1")).thenReturn(pass);
        when(memberSupport.isBlockingRecruitApply(pass)).thenReturn(true);

        AuthUser user = new AuthUser();
        user.setId("M1");
        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(user);

            assertThatThrownBy(() -> service.submitApplication(new DistributionRecruitApplyDTO()))
                    .isInstanceOf(ServiceException.class)
                    .satisfies(ex -> {
                        ServiceException serviceException = (ServiceException) ex;
                        assertThat(serviceException.getResultCode()).isEqualTo(ResultCode.DISTRIBUTION_IS_APPLY);
                    });
        }
    }

    @Test
    void submitApplication_stillThrowsRetreatedWhenOnlineEntryClosed() {
        DistributionRecruitServiceImpl service = spy(new DistributionRecruitServiceImpl());
        DistributionMemberSupport memberSupport = mock(DistributionMemberSupport.class);
        ReflectionTestUtils.setField(service, "distributionMemberSupport", memberSupport);
        injectOpenDistribution(service);

        DistributionRecruitSetting setting = new DistributionRecruitSetting();
        setting.setPlanVersion(1);
        setting.setOnlineEntryVisible(false);
        doReturn(setting).when(service).getSetting();

        Distribution retreated = new Distribution();
        retreated.setMemberId("M1");
        retreated.setDistributionStatus(DistributionStatusEnum.RETREAT.name());
        when(memberSupport.getByMemberId("M1")).thenReturn(retreated);
        when(memberSupport.isBlockingRecruitApply(retreated)).thenReturn(true);

        AuthUser user = new AuthUser();
        user.setId("M1");
        try (MockedStatic<UserContext> userContext = mockStatic(UserContext.class)) {
            userContext.when(UserContext::getCurrentUser).thenReturn(user);

            assertThatThrownBy(() -> service.submitApplication(new DistributionRecruitApplyDTO()))
                    .isInstanceOf(ServiceException.class)
                    .satisfies(ex -> {
                        ServiceException serviceException = (ServiceException) ex;
                        assertThat(serviceException.getResultCode()).isEqualTo(ResultCode.DISTRIBUTION_RETREATED);
                        assertThat(serviceException.getResultCode())
                                .isNotEqualTo(ResultCode.DISTRIBUTION_ONLINE_ENTRY_CLOSED);
                    });
        }
    }

    private static void injectOpenDistribution(DistributionRecruitServiceImpl service) {
        DistributionSettingSupport settingSupport = mock(DistributionSettingSupport.class);
        DistributionSetting setting = new DistributionSetting();
        setting.setIsOpen(true);
        when(settingSupport.getSetting()).thenReturn(setting);
        ReflectionTestUtils.setField(service, "distributionSettingSupport", settingSupport);
    }
}
