package cn.lili.modules.distribution.serviceimpl;

import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dto.DistributionMemberUpdateDTO;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.support.DistributionParentBindSupport;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 管理端清除/改绑上级：Wrapper 写 null，并兜底同步内存对象。
 *
 * @author Mike
 * @date 2026-09-10
 */
class DistributionServiceImplUpdateParentByAdminTest {

    private DistributionServiceImpl service;
    private DistributionParentBindSupport parentBindSupport;
    private MemberService memberService;
    private DistributionCustomerRelationService customerRelationService;
    private DistributionGradeService gradeService;

    @BeforeAll
    static void initDistributionEntityCache() {
        TableInfoHelper.initTableInfo(
                new MapperBuilderAssistant(new MybatisConfiguration(), ""),
                Distribution.class);
    }

    @BeforeEach
    void setUp() {
        service = spy(new DistributionServiceImpl());
        parentBindSupport = mock(DistributionParentBindSupport.class);
        memberService = mock(MemberService.class);
        customerRelationService = mock(DistributionCustomerRelationService.class);
        gradeService = mock(DistributionGradeService.class);
        ReflectionTestUtils.setField(service, "distributionParentBindSupport", parentBindSupport);
        ReflectionTestUtils.setField(service, "memberService", memberService);
        ReflectionTestUtils.setField(service, "customerRelationService", customerRelationService);
        ReflectionTestUtils.setField(service, "gradeService", gradeService);
        doNothing().when(parentBindSupport).syncInviteCountAndGrade(anyString());
    }

    @Test
    void updateParentByAdmin_clearsParentWithWrapperWhenOldParentExists() {
        Distribution child = child("D1", "P1");
        doReturn(child).when(service).getById("D1");
        doReturn(true).when(service).update(any(LambdaUpdateWrapper.class));

        service.updateParentByAdmin("D1", null);

        ArgumentCaptor<LambdaUpdateWrapper<Distribution>> captor =
                ArgumentCaptor.forClass(LambdaUpdateWrapper.class);
        verify(service).update(captor.capture());
        assertThat(captor.getValue().getSqlSet()).contains("parent_distribution_id");
        verify(parentBindSupport).syncInviteCountAndGrade("P1");
        verify(service, never()).updateById(any(Distribution.class));
    }

    @Test
    void updateParentByAdmin_skipsUpdateWhenAlreadyNoParent() {
        Distribution child = child("D1", null);
        doReturn(child).when(service).getById("D1");

        service.updateParentByAdmin("D1", null);

        verify(service, never()).update(any(LambdaUpdateWrapper.class));
        verify(service, never()).updateById(any(Distribution.class));
        verify(parentBindSupport, never()).syncInviteCountAndGrade(anyString());
    }

    @Test
    void updateParentByAdmin_rebindsParentByUpdateById() {
        Distribution child = child("D1", "P1");
        doReturn(child).when(service).getById("D1");
        doReturn(true).when(service).updateById(any(Distribution.class));

        service.updateParentByAdmin("D1", "P2");

        ArgumentCaptor<Distribution> captor = ArgumentCaptor.forClass(Distribution.class);
        verify(service).updateById(captor.capture());
        assertThat(captor.getValue().getParentDistributionId()).isEqualTo("P2");
        verify(service, never()).update(any(LambdaUpdateWrapper.class));
        verify(parentBindSupport).syncInviteCountAndGrade("P1");
        verify(parentBindSupport).syncInviteCountAndGrade("P2");
    }

    @Test
    void updateMember_clearsParentOnEntityBeforeFinalUpdateById() {
        Distribution child = child("D1", "P1");
        child.setMemberId("M1");
        doReturn(child).when(service).getById("D1");
        doReturn(true).when(service).update(any(LambdaUpdateWrapper.class));
        doReturn(true).when(service).updateById(any(Distribution.class));

        Member member = new Member();
        member.setId("M1");
        member.setNickName("原昵称");
        when(memberService.getById("M1")).thenReturn(member);
        when(customerRelationService.getActiveRelation("M1")).thenReturn(null);

        DistributionMemberUpdateDTO updateDTO = new DistributionMemberUpdateDTO();
        updateDTO.setNickname("新昵称");
        updateDTO.setInviterMobile("");

        service.updateMember("D1", updateDTO);

        ArgumentCaptor<Distribution> captor = ArgumentCaptor.forClass(Distribution.class);
        verify(service).updateById(captor.capture());
        assertThat(captor.getValue().getParentDistributionId()).isNull();
    }

    private static Distribution child(String id, String parentId) {
        Distribution distribution = new Distribution();
        distribution.setId(id);
        distribution.setParentDistributionId(parentId);
        return distribution;
    }
}
