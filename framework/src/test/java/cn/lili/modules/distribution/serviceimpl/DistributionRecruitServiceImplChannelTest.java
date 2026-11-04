package cn.lili.modules.distribution.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.support.DistributionMemberSupport;
import cn.lili.modules.system.entity.dto.DistributionRecruitSetting;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 线上招募通道：入口关闭时仅有效邀请人可放行，与总闸 22000 分开。
 * 
 * @author Mike
 * @date 2026-09-13
 */
class DistributionRecruitServiceImplChannelTest {

    /**
     * 验证线上入口关闭且邀请码为空时返回入口关闭错误。
     */
    @Test
    void closedEntry_blankToken_throwsOnlineEntryClosed() {
        DistributionRecruitServiceImpl service = newService();
        DistributionRecruitSetting setting = closedEntry();

        assertThatThrownBy(() -> service.assertOnlineRecruitChannelAllowed(setting, null, "M1"))
                .isInstanceOf(ServiceException.class)
                .satisfies(ex -> assertChannelCode((ServiceException) ex,
                        ResultCode.DISTRIBUTION_ONLINE_ENTRY_CLOSED));
        assertThatThrownBy(() -> service.assertOnlineRecruitChannelAllowed(setting, "  ", "M1"))
                .isInstanceOf(ServiceException.class)
                .satisfies(ex -> assertChannelCode((ServiceException) ex,
                        ResultCode.DISTRIBUTION_ONLINE_ENTRY_CLOSED));
    }

    /**
     * 验证线上入口关闭且邀请码无效时返回邀请无效错误。
     */
    @Test
    void closedEntry_nonBlankInvalidToken_throwsInviteInvalid() {
        DistributionRecruitServiceImpl service = newService();
        DistributionMemberSupport memberSupport = mock(DistributionMemberSupport.class);
        ReflectionTestUtils.setField(service, "distributionMemberSupport", memberSupport);
        when(memberSupport.getById("fake")).thenReturn(null);

        assertThatThrownBy(() -> service.assertOnlineRecruitChannelAllowed(closedEntry(), "fake", "M1"))
                .isInstanceOf(ServiceException.class)
                .satisfies(ex -> {
                    ServiceException serviceException = (ServiceException) ex;
                    assertThat(serviceException.getResultCode())
                            .isEqualTo(ResultCode.DISTRIBUTION_RECRUIT_INVITE_INVALID);
                    assertThat(serviceException.getResultCode())
                            .isNotEqualTo(ResultCode.DISTRIBUTION_ONLINE_ENTRY_CLOSED);
                    assertThat(serviceException.getResultCode())
                            .isNotEqualTo(ResultCode.DISTRIBUTION_CLOSE);
                });
    }

    /**
     * 验证线上入口关闭时仍允许有效邀请渠道并返回邀请人ID。
     */
    @Test
    void closedEntry_validInviter_returnsInviterId() {
        DistributionRecruitServiceImpl service = newService();
        DistributionMemberSupport memberSupport = mock(DistributionMemberSupport.class);
        ReflectionTestUtils.setField(service, "distributionMemberSupport", memberSupport);

        when(memberSupport.getById("D1")).thenReturn(passInviter("D1", "M2"));

        String inviterId = service.assertOnlineRecruitChannelAllowed(closedEntry(), "D1", "M1");
        assertThat(inviterId).isEqualTo("D1");
    }

    /**
     * 验证展示配置为空且无邀请码时按入口关闭处理。
     */
    @Test
    void closedEntry_nullVisible_blankToken_throwsOnlineEntryClosed() {
        DistributionRecruitServiceImpl service = newService();
        DistributionRecruitSetting setting = new DistributionRecruitSetting();

        assertThatThrownBy(() -> service.assertOnlineRecruitChannelAllowed(setting, null, "M1"))
                .isInstanceOf(ServiceException.class)
                .satisfies(ex -> assertChannelCode((ServiceException) ex,
                        ResultCode.DISTRIBUTION_ONLINE_ENTRY_CLOSED));
    }

    /**
     * 验证会员不能使用自己的邀请渠道申请。
     */
    @Test
    void closedEntry_selfInvite_throwsInviteInvalid() {
        DistributionRecruitServiceImpl service = newService();
        DistributionMemberSupport memberSupport = mock(DistributionMemberSupport.class);
        ReflectionTestUtils.setField(service, "distributionMemberSupport", memberSupport);

        Distribution self = passInviter("D1", "M1");
        when(memberSupport.getById("D1")).thenReturn(self);

        assertThatThrownBy(() -> service.assertOnlineRecruitChannelAllowed(closedEntry(), "D1", "M1"))
                .isInstanceOf(ServiceException.class)
                .satisfies(ex -> assertChannelCode((ServiceException) ex,
                        ResultCode.DISTRIBUTION_RECRUIT_INVITE_INVALID));
    }

    /**
     * 验证未通过审核的邀请人不能提供有效邀请渠道。
     */
    @Test
    void closedEntry_nonPassInviter_throwsInviteInvalid() {
        DistributionRecruitServiceImpl service = newService();
        DistributionMemberSupport memberSupport = mock(DistributionMemberSupport.class);
        ReflectionTestUtils.setField(service, "distributionMemberSupport", memberSupport);

        Distribution refuse = new Distribution();
        refuse.setId("D1");
        refuse.setMemberId("M2");
        refuse.setDistributionStatus(DistributionStatusEnum.REFUSE.name());
        when(memberSupport.getById("D1")).thenReturn(refuse);

        assertThatThrownBy(() -> service.assertOnlineRecruitChannelAllowed(closedEntry(), "D1", "M1"))
                .isInstanceOf(ServiceException.class)
                .satisfies(ex -> assertChannelCode((ServiceException) ex,
                        ResultCode.DISTRIBUTION_RECRUIT_INVITE_INVALID));
    }

    /**
     * 验证线上入口开放且无邀请码时允许直接申请。
     */
    @Test
    void openEntry_blankToken_returnsNullWithoutThrowing() {
        DistributionRecruitServiceImpl service = newService();
        DistributionRecruitSetting setting = new DistributionRecruitSetting();
        setting.setOnlineEntryVisible(true);

        assertThat(service.assertOnlineRecruitChannelAllowed(setting, null, "M1")).isNull();
    }

    /**
     * 创建分销招募服务并注入测试依赖。
     *
     * @return 已注入测试依赖的分销招募服务
     */
    private static DistributionRecruitServiceImpl newService() {
        DistributionRecruitServiceImpl service = new DistributionRecruitServiceImpl();
        ReflectionTestUtils.setField(service, "distributionMemberSupport", mock(DistributionMemberSupport.class));
        return service;
    }

    /**
     * 构造审核通过的邀请人记录。
     *
     * @param distributionId 分销员ID
     * @param memberId       会员ID
     * @return 审核通过的邀请人记录
     */
    private static Distribution passInviter(String distributionId, String memberId) {
        Distribution inviter = new Distribution();
        inviter.setId(distributionId);
        inviter.setMemberId(memberId);
        inviter.setDistributionStatus(DistributionStatusEnum.PASS.name());
        return inviter;
    }

    /**
     * 构造关闭线上招募入口的分销配置。
     *
     * @return 关闭线上招募入口的分销配置
     */
    private static DistributionRecruitSetting closedEntry() {
        DistributionRecruitSetting setting = new DistributionRecruitSetting();
        setting.setOnlineEntryVisible(false);
        return setting;
    }

    /**
     * 执行渠道校验并断言业务错误码。
     *
     * @param ex       待校验异常
     * @param expected 预期结果
     */
    private static void assertChannelCode(ServiceException ex, ResultCode expected) {
        assertThat(ex.getResultCode()).isEqualTo(expected);
        assertThat(ex.getResultCode()).isNotEqualTo(ResultCode.DISTRIBUTION_CLOSE);
        assertThat(ex.getResultCode()).isNotEqualTo(ResultCode.DISTRIBUTION_IS_APPLY);
        assertThat(ex.getResultCode()).isNotEqualTo(ResultCode.DISTRIBUTION_RECRUIT_INVITE_INVALID.equals(expected)
                ? ResultCode.DISTRIBUTION_ONLINE_ENTRY_CLOSED
                : ResultCode.DISTRIBUTION_RECRUIT_INVITE_INVALID);
    }
}
