package cn.lili.modules.distribution.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionRecruitApplication;
import cn.lili.modules.distribution.entity.dto.DistributionRecruitApplyDTO;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.entity.enums.RecruitApplyModeEnum;
import cn.lili.modules.distribution.entity.enums.RecruitJoinConditionTypeEnum;
import cn.lili.modules.distribution.entity.vos.DistributionRecruitConditionProgressVO;
import cn.lili.modules.distribution.mapper.DistributionRecruitApplicationMapper;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.support.DistributionMemberSupport;
import cn.lili.modules.distribution.support.DistributionRecruitPendingTokenSupport;
import cn.lili.modules.distribution.support.DistributionRecruitSettingSupport;
import cn.lili.modules.distribution.support.DistributionSettingSupport;
import cn.lili.modules.system.entity.dto.DistributionRecruitSetting;
import cn.lili.modules.system.entity.dto.DistributionSetting;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatCode;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 招募自动申请：前置过滤、协议自动同意、幂等与预期失败不外抛。
 *
 * @author Mike
 * @date 2026-09-18
 */
class DistributionRecruitServiceImplTryAutoApplyTest {

    private DistributionRecruitServiceImpl service;
    private DistributionMemberSupport memberSupport;
    private DistributionRecruitSettingSupport recruitSettingSupport;
    private DistributionRecruitPendingTokenSupport pendingTokenSupport;

    @BeforeEach
    void setUp() {
        service = spy(new DistributionRecruitServiceImpl());
        memberSupport = mock(DistributionMemberSupport.class);
        recruitSettingSupport = mock(DistributionRecruitSettingSupport.class);
        DistributionSettingSupport distributionSettingSupport = mock(DistributionSettingSupport.class);
        pendingTokenSupport = mock(DistributionRecruitPendingTokenSupport.class);
        DistributionCustomerRelationService customerRelationService = mock(DistributionCustomerRelationService.class);
        when(customerRelationService.getActiveRelation("M1")).thenReturn(null);

        ReflectionTestUtils.setField(service, "distributionMemberSupport", memberSupport);
        ReflectionTestUtils.setField(service, "recruitSettingSupport", recruitSettingSupport);
        ReflectionTestUtils.setField(service, "distributionSettingSupport", distributionSettingSupport);
        ReflectionTestUtils.setField(service, "pendingTokenSupport", pendingTokenSupport);
        ReflectionTestUtils.setField(service, "customerRelationService", customerRelationService);

        DistributionSetting open = new DistributionSetting();
        open.setIsOpen(true);
        when(distributionSettingSupport.getSetting()).thenReturn(open);
        when(memberSupport.getByMemberId("M1")).thenReturn(null);
        when(memberSupport.isBlockingRecruitApply(any())).thenReturn(false);
        doReturn(null).when(service).getPendingApplicationByMemberId("M1");
        doReturn(satisfiedProgress()).when(service).getConditionProgress("M1");
    }

    /**
     * AUTO 且条件已达标时写入自动入驻申请，并清掉 pending token。
     */
    @Test
    void tryAutoApply_autoSatisfied_createsAutoEntry() {
        DistributionRecruitSetting setting = autoSetting(null);
        doReturn(setting).when(service).getSetting();
        DistributionRecruitApplicationMapper mapper = mock(DistributionRecruitApplicationMapper.class);
        ReflectionTestUtils.setField(service, "applicationMapper", mapper);
        ArgumentCaptor<DistributionRecruitApplication> captor =
                ArgumentCaptor.forClass(DistributionRecruitApplication.class);
        when(mapper.insert(captor.capture())).thenReturn(1);

        service.tryAutoApply("M1");

        assertThat(captor.getValue().getApplySource()).isEqualTo("AUTO_ENTRY");
        assertThat(captor.getValue().getMemberId()).isEqualTo("M1");
        verify(pendingTokenSupport).clear("M1");
    }

    /**
     * MANUAL 模式不自动提交申请。
     */
    @Test
    void tryAutoApply_manual_doesNotSubmit() {
        DistributionRecruitSetting setting = autoSetting(null);
        setting.setApplyMode(RecruitApplyModeEnum.MANUAL.name());

        service.tryAutoApply("M1");

        verify(service, never()).submitApplicationForMember(any(), any());
    }

    /**
     * 配置了协议时自动勾选同意。
     */
    @Test
    void tryAutoApply_withAgreement_autoAccepts() {
        autoSetting("协议正文");
        doReturn(new DistributionRecruitApplication()).when(service).submitApplicationForMember(eq("M1"), any());

        service.tryAutoApply("M1");

        ArgumentCaptor<DistributionRecruitApplyDTO> captor = ArgumentCaptor.forClass(DistributionRecruitApplyDTO.class);
        verify(service).submitApplicationForMember(eq("M1"), captor.capture());
        assertThat(captor.getValue().getAgreementAccepted()).isTrue();
    }

    /**
     * 清退、已通过、审核中、已有待审申请时均跳过自动提交。
     */
    @Test
    void tryAutoApply_retreatPassApplyPending_skips() {
        autoSetting(null);
        Distribution retreated = status(DistributionStatusEnum.RETREAT);
        when(memberSupport.getByMemberId("M1")).thenReturn(retreated);

        service.tryAutoApply("M1");
        verify(service, never()).submitApplicationForMember(any(), any());

        Distribution pass = status(DistributionStatusEnum.PASS);
        when(memberSupport.getByMemberId("M1")).thenReturn(pass);
        when(memberSupport.isBlockingRecruitApply(pass)).thenReturn(true);
        service.tryAutoApply("M1");
        verify(service, never()).submitApplicationForMember(any(), any());

        Distribution apply = status(DistributionStatusEnum.APPLY);
        when(memberSupport.getByMemberId("M1")).thenReturn(apply);
        when(memberSupport.isBlockingRecruitApply(apply)).thenReturn(true);
        service.tryAutoApply("M1");
        verify(service, never()).submitApplicationForMember(any(), any());

        when(memberSupport.getByMemberId("M1")).thenReturn(null);
        when(memberSupport.isBlockingRecruitApply(any())).thenReturn(false);
        doReturn(new DistributionRecruitApplication()).when(service).getPendingApplicationByMemberId("M1");
        service.tryAutoApply("M1");
        verify(service, never()).submitApplicationForMember(any(), any());
    }

    /**
     * 线上入口关闭且无 pending token 时吞掉异常、不外抛。
     */
    @Test
    void tryAutoApply_closedEntryWithoutToken_doesNotThrow() {
        autoSetting(null);
        doThrow(new ServiceException(ResultCode.DISTRIBUTION_ONLINE_ENTRY_CLOSED))
                .when(service).submitApplicationForMember(eq("M1"), any());

        assertThatCode(() -> service.tryAutoApply("M1")).doesNotThrowAnyException();
        verify(service).submitApplicationForMember(eq("M1"), any());
    }

    /**
     * 线上入口关闭但有 pending token 时，将 token 传给提交申请。
     */
    @Test
    void tryAutoApply_closedEntryWithPendingToken_passesToken() {
        autoSetting(null);
        when(pendingTokenSupport.get("M1")).thenReturn("D1");
        doReturn(new DistributionRecruitApplication()).when(service).submitApplicationForMember(eq("M1"), any());

        service.tryAutoApply("M1");

        ArgumentCaptor<DistributionRecruitApplyDTO> captor = ArgumentCaptor.forClass(DistributionRecruitApplyDTO.class);
        verify(service).submitApplicationForMember(eq("M1"), captor.capture());
        assertThat(captor.getValue().getRecruitToken()).isEqualTo("D1");
    }

    /**
     * 方案版本为 0 时不自动申请。
     */
    @Test
    void tryAutoApply_planVersionZero_skips() {
        DistributionRecruitSetting setting = autoSetting(null);
        setting.setPlanVersion(0);

        service.tryAutoApply("M1");

        verify(service, never()).submitApplicationForMember(any(), any());
    }

    /**
     * 连续多次触发只提交一次，保证幂等。
     */
    @Test
    void tryAutoApply_repeatedCalls_submitsOnce() {
        autoSetting(null);
        AtomicInteger submitted = new AtomicInteger();
        doAnswer(invocation -> submitted.get() > 0 ? new DistributionRecruitApplication() : null)
                .when(service).getPendingApplicationByMemberId("M1");
        doAnswer(invocation -> {
            submitted.incrementAndGet();
            return new DistributionRecruitApplication();
        }).when(service).submitApplicationForMember(eq("M1"), any());

        for (int i = 0; i < 10; i++) {
            service.tryAutoApply("M1");
        }
        verify(service, times(1)).submitApplicationForMember(eq("M1"), any());
    }

    private DistributionRecruitSetting autoSetting(String agreement) {
        DistributionRecruitSetting setting = new DistributionRecruitSetting();
        setting.setApplyMode(RecruitApplyModeEnum.AUTO.name());
        setting.setPlanVersion(1);
        setting.setJoinConditionType(RecruitJoinConditionTypeEnum.NONE.name());
        setting.setOnlineEntryVisible(true);
        setting.setAgreementContent(agreement);
        when(recruitSettingSupport.readSetting()).thenReturn(setting);
        return setting;
    }

    private static DistributionRecruitConditionProgressVO satisfiedProgress() {
        DistributionRecruitConditionProgressVO progress = new DistributionRecruitConditionProgressVO();
        progress.setSatisfied(true);
        return progress;
    }

    private static Distribution status(DistributionStatusEnum status) {
        Distribution distribution = new Distribution();
        distribution.setDistributionStatus(status.name());
        return distribution;
    }
}
