package cn.lili.modules.distribution.support;

import cn.lili.common.exception.ServiceException;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.entity.dto.DistributionRecruitApplyFormItemSnapshot;
import cn.lili.modules.distribution.entity.vos.DistributionMemberVO;
import cn.lili.modules.distribution.mapper.DistributionCustomerRelationMapper;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.service.DistributionRecruitService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 分销员管理端支持类单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
class DistributionMemberAdminSupportTest {

    private MemberService memberService;
    private DistributionGradeService gradeService;
    private DistributionRecruitService recruitService;
    private DistributionCustomerRelationService customerRelationService;
    private DistributionCustomerRelationMapper customerRelationMapper;
    private DistributionMemberSupport memberSupport;
    private DistributionService distributionService;

    private DistributionMemberAdminSupport adminSupport;

    @BeforeEach
    void setUp() {
        memberService = mock(MemberService.class);
        gradeService = mock(DistributionGradeService.class);
        recruitService = mock(DistributionRecruitService.class);
        customerRelationService = mock(DistributionCustomerRelationService.class);
        customerRelationMapper = mock(DistributionCustomerRelationMapper.class);
        memberSupport = mock(DistributionMemberSupport.class);
        distributionService = mock(DistributionService.class);

        adminSupport = new DistributionMemberAdminSupport(
                memberService,
                gradeService,
                recruitService,
                customerRelationService,
                customerRelationMapper,
                memberSupport
        );
    }

    @Test
    @DisplayName("测试分销员实体转换为VO")
    void testToMemberVO() {
        Distribution distribution = new Distribution();
        distribution.setId("D1");
        distribution.setMemberId("M1");
        distribution.setGradeId("G1");
        distribution.setParentDistributionId("P1");
        distribution.setCanRebate(100.0);

        Member member = new Member();
        member.setId("M1");
        member.setMobile("13800000001");
        member.setNickName("会员1");
        when(memberService.getById("M1")).thenReturn(member);

        DistributionGrade grade = new DistributionGrade();
        grade.setId("G1");
        grade.setGradeName("初级分销员");
        when(gradeService.resolveGrade("G1")).thenReturn(grade);

        Distribution parentDist = new Distribution();
        parentDist.setId("P1");
        parentDist.setMemberId("PM1");
        parentDist.setMemberName("上级分销员");
        when(distributionService.getById("P1")).thenReturn(parentDist);

        Member parentMember = new Member();
        parentMember.setId("PM1");
        parentMember.setMobile("13800000002");
        when(memberService.getById("PM1")).thenReturn(parentMember);

        DistributionMemberVO vo = adminSupport.toMemberVO(distributionService, distribution);

        assertNotNull(vo);
        assertEquals("D1", vo.getId());
        assertEquals("13800000001", vo.getMobile());
        assertEquals("初级分销员", vo.getGradeName());
        assertEquals("P1", vo.getInviterDistributionId());
        assertEquals("13800000002", vo.getInviterMobile());
    }

    @Test
    @DisplayName("测试邀请关系成环检测")
    void testValidateInviteCycle() {
        // 自邀请
        assertThrows(ServiceException.class, () ->
                adminSupport.validateInviteCycle(distributionService, "D1", "D1"));

        // 下级邀请上级成环 (D1 -> D2 -> D1)
        Distribution d2 = new Distribution();
        d2.setId("D2");
        d2.setParentDistributionId("D1");
        when(distributionService.getById("D2")).thenReturn(d2);

        assertThrows(ServiceException.class, () ->
                adminSupport.validateInviteCycle(distributionService, "D1", "D2"));
    }

    @Test
    @DisplayName("测试申请表单快照解析")
    void testParseApplyFormItems() {
        String json = "[{\"fieldName\":\"realName\",\"fieldValue\":\"张三\"}]";
        List<DistributionRecruitApplyFormItemSnapshot> items = adminSupport.parseApplyFormItems(json);

        assertNotNull(items);
        assertEquals(1, items.size());
        assertEquals("realName", items.get(0).getFieldName());
        assertEquals("张三", items.get(0).getFieldValue());

        List<DistributionRecruitApplyFormItemSnapshot> emptyItems = adminSupport.parseApplyFormItems(null);
        assertNotNull(emptyItems);
        assertEquals(0, emptyItems.size());
    }

    @Test
    @DisplayName("测试金额默认值转换")
    void testDefaultAmount() {
        assertEquals(BigDecimal.ZERO, adminSupport.defaultAmount(null));
        assertEquals(BigDecimal.TEN, adminSupport.defaultAmount(BigDecimal.TEN));
    }
}
